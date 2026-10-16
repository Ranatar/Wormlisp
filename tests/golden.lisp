;;;; tests/golden.lisp — эталонные прогоны всех уровней управления (этап 13).
;;;;
;;;; Архитектура, «Тестирование»: небольшой набор прогонов фиксируется
;;;; конфигурацией и хэшами frames.bin. Благодаря детерминизму сверка
;;;; побитовая; при расхождении тест называет первый отличающийся кадр.
;;;; Изменение, которое законно меняет эталон, обновляется явной командой
;;;; (update-golden :<имя> :reason "…"), и причина пишется в журнал
;;;; tests/golden/journal.txt. Между системами с разной libm эталон может
;;;; разойтись (архитектура, «Детерминизм»).

(in-package #:worm.tests)

(defun golden-dir () (merge-pathnames "tests/golden/" (worm.core:project-root)))

(defun golden-specs ()
  "((имя . plist) …) из tests/golden/goldens.sexp."
  (with-open-file (in (merge-pathnames "goldens.sexp" (golden-dir)) :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*read-eval* nil) (*package* (find-package :worm.tests)) (*read-default-float-format* 'double-float))
        (loop for form = (read in nil :eof) until (eq form :eof)
              collect (destructuring-bind (head name &rest plist) form
                        (assert (string= (symbol-name head) "GOLDEN"))
                        (cons (intern (symbol-name name) :keyword) plist)))))))

(defun golden-record-path (name) (merge-pathnames (format nil "~(~A~).sexp" name) (golden-dir)))

(defun run-golden (name)
  "Посчитать эталон NAME: (values md5-файла список-md5-кадров число-кадров)."
  (let* ((spec (or (cdr (assoc name (golden-specs))) (error "эталона ~S нет" name)))
         (model (worm.codegen:compile-model (find-symbol (symbol-name (getf spec :model)) :worm.components)
                                            :params (getf spec :params) :cache t))
         (dir (runtime-dir (format nil "golden-~(~A~)" name)))
         (run (worm.runtime:start-run model :params (getf spec :params) :dt (getf spec :dt) :paused t
                                            :record t :frames-every (getf spec :frames-every) :directory dir
                                            :policy :batch :thread nil)))
    (worm.runtime:step-run (getf spec :steps) run)
    (worm.runtime:stop-run run)
    (let* ((frames (worm.io:read-frames-file (merge-pathnames "frames.bin" dir)))
           (octets (worm.io::frames-file-octets frames))
           (size (worm.io::frames-file-frame-size frames))
           (count (worm.io:frames-file-count frames)))
      (values (worm.platform:md5-octets-hex octets)
              (loop for k below count
                    for start = (+ worm.io::+file-header-size+ (* k size))
                    collect (worm.platform:md5-octets-hex octets :start start :end (+ start size)))
              count))))

(defun read-golden-record (name)
  (let ((path (golden-record-path name)))
    (and (probe-file path)
         (with-open-file (in path :external-format :utf-8)
           (with-standard-io-syntax (let ((*read-eval* nil)) (read in)))))))

(defun update-golden (name &key reason)
  "Пересчитать эталон NAME и записать его хэши; REASON обязателен и
пишется в журнал tests/golden/journal.txt."
  (unless (and (stringp reason) (plusp (length reason)))
    (error "обновление эталона ~S требует причины: :reason \"…\"" name))
  (multiple-value-bind (md5 frame-md5 count) (run-golden name)
    (let ((old (read-golden-record name)))
      (with-open-file (out (golden-record-path name) :direction :output :if-exists :supersede
                                                       :external-format :utf-8)
        (with-standard-io-syntax
          (let ((*print-readably* nil) (*print-right-margin* 100))
            (format out ";;;; Эталон ~(~A~): хэши frames.bin. Порождён update-golden, не править руками.~%" name)
            (prin1 (list :name (string-downcase name) :md5 md5 :frames count
                         :revision (or (worm.core:git-revision) "unknown") :frame-md5 frame-md5)
                   out)
            (terpri out))))
      (with-open-file (out (merge-pathnames "journal.txt" (golden-dir)) :direction :output :if-exists :append
                                                                       :if-does-not-exist :create
                                                                       :external-format :utf-8)
        (format out "~A ~(~A~) ~A → ~A, кадров ~D: ~A~%"
                (worm.batch::utc-now) name (or (getf old :md5) "нет") md5 count reason))
      md5)))

(in-suite physics)

(test golden-runs-match
  (dolist (spec (golden-specs))
    (let* ((name (car spec)) (record (read-golden-record name)))
      (is (not (null record)) "эталон ~(~A~) не записан: (update-golden ~S :reason \"…\")" name name)
      (when record
        (multiple-value-bind (md5 frame-md5 count) (run-golden name)
          (is (= (getf record :frames) count) "~(~A~): кадров ~D, в эталоне ~D" name count (getf record :frames))
          (is (string= (getf record :md5) md5)
              "~(~A~): frames.bin разошёлся с эталоном; первый отличающийся кадр — ~A"
              name (or (position-if-not #'identity (mapcar #'string= frame-md5 (getf record :frame-md5))) "заголовок")))))))

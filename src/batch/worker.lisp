;;;; src/batch/worker.lisp — рабочий процесс: одно задание.
;;;;
;;;; Рабочий собирает модель (fasl из кэша, если есть), строит прогон без
;;;; сервера и без потока симуляции — с набором параметров, где правки
;;;; задания уже применены (поэтому меняются и параметры без живой правки), —
;;;; считает и пишет результат во временный
;;;; каталог runs/<id>.tmp/. Успех — переименование в runs/<id>/ и статус
;;;; done; сбой — failed и отчёт об условии в log.txt. Ошибки решает
;;;; пакетная политика: отладчик не открывается никогда.

(in-package #:worm.batch)

(defun runs-root () (worm.io:runs-directory))

(defun run-directory (id) (merge-pathnames (format nil "~A/" id) (runs-root)))
(defun tmp-directory (id) (merge-pathnames (format nil "~A.tmp/" id) (runs-root)))

(defun job-status (id)
  (let ((dir (run-directory id)))
    (and (probe-file dir) (worm.io:read-status dir))))

(defun utc-now ()
  (multiple-value-bind (s mi h d mo y) (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" y mo d h mi s)))

(defun find-model-symbol (name)
  (or (find-symbol name :worm.components)
      (batch-error "модели ~A в worm.components нет" name)))

(defun write-final-checkpoint (run)
  "Состояние в конце прогона — для побитовой сверки с тем же прогоном в REPL."
  (worm.runtime:save-checkpoint run))

(defun run-job (path &key after)
  "Посчитать задание из файла PATH. AFTER — функция от временного каталога,
вызывается после расчёта до переименования (метрики). Возвращает статус."
  (let* ((job (read-job path))
         (id (getf job :id))
         (tmp (tmp-directory id))
         (final (run-directory id))
         (run nil))
    (uiop:delete-directory-tree tmp :validate t :if-does-not-exist :ignore)
    (handler-case
        (let* ((set (worm.params:derive-param-set
                     (worm.params:load-param-set (let ((p (getf job :params)))
                                                   (if (find #\/ p) (project-path p) p)))
                     (format nil "job-~A" id)
                     (getf job :set)))
               (model (worm.codegen:compile-model (find-model-symbol (getf job :model))
                                                  :params set :medium (getf job :medium)
                                                  :rings (getf job :rings) :cache t))
               (dt (getf job :dt))
               (record (getf job :record)))
          (setf run (worm.runtime:start-run model :params set :dt dt :id id :directory tmp
                                                  :record (and record t)
                                                  :frames-every (getf record :frames-every 0.01d0)
                                                  :policy :batch :thread nil))
          (worm.runtime:step-run (round (getf job :duration) dt) run)
          (unless (eq (worm.runtime:run-status run) :failed)
            (write-final-checkpoint run))
          (worm.runtime:stop-run run))
      (error (e)
        (ignore-errors (when run (worm.runtime:abort-run-with run (princ-to-string e))))
        (ensure-directories-exist tmp)
        (with-open-file (out (merge-pathnames "log.txt" tmp) :direction :output :if-exists :append
                                                             :if-does-not-exist :create :external-format :utf-8)
          (format out "~A: задание не выполнено: ~A~%" (utc-now) e))
        (worm.io:write-status tmp "failed")))
    (when (and after (string= (or (worm.io:read-status tmp) "") "done"))
      (handler-case (funcall after tmp job)
        (error (e)
          (with-open-file (out (merge-pathnames "log.txt" tmp) :direction :output :if-exists :append
                                                               :if-does-not-exist :create :external-format :utf-8)
            (format out "~A: метрики не посчитаны: ~A~%" (utc-now) e)))))
    (uiop:delete-directory-tree final :validate t :if-does-not-exist :ignore)
    (worm.platform:rename-directory tmp final)
    (worm.io:read-status final)))

(defun worker-main (path &key after)
  "Точка входа рабочего: посчитать задание и выйти с кодом 0 (done) или 1."
  (let ((status (handler-case (run-job path :after after)
                  (error (e) (format *error-output* "~&задание ~A: ~A~%" path e) "failed"))))
    (finish-output)
    (sb-ext:exit :code (if (equal status "done") 0 1) :abort t)))

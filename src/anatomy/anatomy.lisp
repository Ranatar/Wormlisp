;;;; src/anatomy/anatomy.lisp — файл мышц, проверка, привязка к рёбрам.
;;;;
;;;; Формат anatomy/muscles.sexp (s-выражения, читаются с *read-eval* = NIL):
;;;;
;;;;   (muscle-layout :head-start 0.02 :vulva 0.57 :tail-end 0.95 :source "...")
;;;;   (muscle mdl01 :quadrant :dorsal-left :row :medial :index 1
;;;;                 :from 0.020000 :peak 0.054375 :to 0.088750)
;;;;   ...
;;;;
;;;; Положения — доли длины тела, поэтому файл не зависит от числа колец.
;;;; Файл порождается по правилу раскладки скриптом scripts/make-muscles.lisp,
;;;; но читается как самостоятельный источник: правка руками допустима, а
;;;; тест сообщает, если файл разошёлся с правилом.

(in-package #:worm.anatomy)

(define-condition anatomy-invalid (model-error) ()
  (:documentation "Файл анатомии не читается или нарушает её правила."))

(defun invalid (control &rest args)
  (error 'anatomy-invalid :message (apply #'format nil control args)))

(defstruct (anatomy (:copier nil))
  (muscles #() :type simple-vector :read-only t)
  (layout nil :read-only t)
  (path nil :read-only t)
  (neurons nil :read-only t)  ; Этап 12.
  (synapses nil :read-only t))  ; Этап 12.

(defmethod print-object ((a anatomy) stream)
  (print-unreadable-object (a stream :type t)
    (format stream "~D мышц~@[, ~A~]" (length (anatomy-muscles a))
            (and (anatomy-path a) (file-namestring (anatomy-path a))))))

(defun default-muscles-path ()
  (merge-pathnames "anatomy/muscles.sexp" (project-root)))

(defun find-muscle (anatomy name)
  (find (intern (string-upcase (string name)) :keyword) (anatomy-muscles anatomy)
        :key #'muscle-name))

;;; Чтение и запись

(defpackage #:worm.anatomy.reader (:use #:cl))

(defun read-forms (path)
  (with-open-file (in path :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*read-eval* nil)
            (*read-default-float-format* 'double-float)
            (*package* (find-package '#:worm.anatomy.reader)))
        (handler-case
            (loop for form = (read in nil :eof) until (eq form :eof) collect form)
          (reader-error (e) (invalid "~A не читается: ~A" (file-namestring path) e)))))))

(defun head-is (form name)
  (and (consp form) (symbolp (first form)) (string= (symbol-name (first form)) name)))

(defun plist-number (plist key what)
  (let ((v (getf plist key)))
    (unless (realp v) (invalid "~A: ~(~S~) должно быть числом, а не ~S" what key v))
    (float v 0d0)))

(defun parse-muscle (form)
  (destructuring-bind (head name &rest plist) form
    (declare (ignore head))
    (unless (and name (symbolp name)) (invalid "у мышцы нет имени: ~S" form))
    (unless (evenp (length plist)) (invalid "~(~A~): нечётный список свойств" name))
    (let ((quadrant (getf plist :quadrant))
          (row (getf plist :row))
          (index (getf plist :index)))
      (unless (member quadrant +muscle-quadrants+)
        (invalid "~(~A~): квадрант ~S не из ~{~(~S~)~^ ~}" name quadrant +muscle-quadrants+))
      (unless (member row '(:medial :lateral)) (invalid "~(~A~): ряд ~S" name row))
      (unless (and (integerp index) (<= 1 index 24)) (invalid "~(~A~): номер ~S" name index))
      (make-muscle :name (intern (symbol-name name) :keyword)
                   :quadrant quadrant :row row :index index
                   :from (plist-number plist :from name)
                   :peak (plist-number plist :peak name)
                   :to (plist-number plist :to name)))))

(defun read-muscles-file (path)
  "Прочитать файл мышц: (values вектор-мышц раскладка-или-NIL)."
  (let ((layout nil) (muscles '()))
    (dolist (form (read-forms path))
      (cond ((head-is form "MUSCLE-LAYOUT")
             (when layout (invalid "в файле две формы muscle-layout"))
             (let ((plist (rest form)))
               (setf layout (make-muscle-layout
                             :head-start (plist-number plist :head-start "muscle-layout")
                             :vulva (plist-number plist :vulva "muscle-layout")
                             :tail-end (plist-number plist :tail-end "muscle-layout")
                             :source (or (getf plist :source) "")))))
            ((head-is form "MUSCLE") (push (parse-muscle form) muscles))
            (t (invalid "неизвестная форма в файле мышц: ~S" form))))
    (values (coerce (nreverse muscles) 'simple-vector) layout)))

(defun write-muscles-file (path muscles layout &key (comment ""))
  "Записать мышцы MUSCLES и правило LAYOUT в файл PATH."
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*print-case* :downcase))
        (format out "~A" comment)
        (format out "(muscle-layout :head-start ~,4F :vulva ~,4F :tail-end ~,4F~%  :source ~S)~%"
                (muscle-layout-head-start layout) (muscle-layout-vulva layout)
                (muscle-layout-tail-end layout) (muscle-layout-source layout))
        (let ((quadrant nil))
          (loop for m across (coerce muscles 'simple-vector)
                do (unless (eq quadrant (muscle-quadrant m))
                     (setf quadrant (muscle-quadrant m))
                     (format out "~%;; ~(~A~)~%" quadrant))
                   (format out "(muscle ~(~A~) :quadrant ~(~S~) :row ~(~S~) :index ~D :from ~,6F :peak ~,6F :to ~,6F)~%"
                           (muscle-name m) (muscle-quadrant m) (muscle-row m) (muscle-index m)
                           (muscle-from m) (muscle-peak m) (muscle-to m))))))
    path))

;;; Проверка

(defun validate-muscles (muscles)
  "Проверить набор мышц по правилам анатомии; вернуть MUSCLES или сигнализировать anatomy-invalid."
  (let ((names (make-hash-table :test 'eq)))
    (loop for m across muscles
          do (when (gethash (muscle-name m) names)
               (invalid "мышца ~(~A~) встречается дважды" (muscle-name m)))
             (setf (gethash (muscle-name m) names) t)
             (unless (eq (muscle-name m) (muscle-name-for (muscle-quadrant m) (muscle-index m)))
               (invalid "мышца ~(~A~) названа не по квадранту и номеру (ожидалось ~(~A~))"
                        (muscle-name m) (muscle-name-for (muscle-quadrant m) (muscle-index m))))
             (unless (eq (muscle-row m) (if (oddp (muscle-index m)) :medial :lateral))
               (invalid "~(~A~): нечётные клетки — медиальный ряд, чётные — латеральный"
                        (muscle-name m)))
             (unless (<= 0d0 (muscle-from m) (muscle-peak m) (muscle-to m) 1d0)
               (invalid "~(~A~): нужно 0 ≤ from ≤ peak ≤ to ≤ 1" (muscle-name m)))
             (unless (< (muscle-from m) (muscle-to m))
               (invalid "~(~A~): клетка нулевой длины" (muscle-name m))))
    (dolist (quadrant +muscle-quadrants+)
      (let* ((cells (sort (remove quadrant (coerce muscles 'list) :key #'muscle-quadrant :test-not #'eq)
                          #'< :key #'muscle-index))
             (expected (getf +muscles-per-quadrant+ quadrant)))
        (unless (= (length cells) expected)
          (invalid "в квадранте ~(~A~) ~D клеток, а должно быть ~D" quadrant (length cells) expected))
        (loop for m in cells for k from 1
              unless (= (muscle-index m) k)
                do (invalid "в квадранте ~(~A~) пропущена клетка ~D" quadrant k))
        (loop for (a b) on cells while b
              unless (< (muscle-peak a) (muscle-peak b))
                do (invalid "~(~A~) лежит не позади ~(~A~)" (muscle-name b) (muscle-name a)))))
    (unless (= (length muscles) 95)
      (invalid "мышц ~D, а должно быть 95" (length muscles)))
    muscles))

(defun load-anatomy (&key (muscles-path (default-muscles-path)))
  "Загрузить анатомию. Нейроны и синапсы появятся на этапе 12.
Перезапуски: RELOAD-ANATOMY — перечитать файл после правки, USE-VALUE — подставить анатомию."
  (loop
    (restart-case
        (multiple-value-bind (muscles layout) (read-muscles-file muscles-path)
          (validate-muscles muscles)
          (log-event :info 'anatomy "загружено ~D мышц из ~A" (length muscles)
                     (file-namestring muscles-path))
          (return (make-anatomy :muscles muscles :layout layout :path muscles-path)))
      (reload-anatomy ()
        :report "Перечитать файл анатомии."
        nil)
      (use-value (anatomy)
        :report "Подставить готовую анатомию."
        (return anatomy)))))

(defun reload-anatomy (&optional condition)
  (let ((restart (find-restart 'reload-anatomy condition)))
    (when restart (invoke-restart restart))))

;;; Привязка к продольным рёбрам

(defstruct (muscle-binding (:copier nil))
  (muscles #() :type simple-vector :read-only t)
  (offsets nil :type ivec :read-only t)  ; Рёбра мышцы m — с offsets[m] до offsets[m+1].
  (edges nil :type ivec :read-only t)  ; Номера продольных рёбер.
  (weights nil :type dvec :read-only t))  ; Доли силы мышцы на ребре.

(defun profile-integral (muscle lo hi)
  "∫ профиля клетки по [LO, HI]; профиль кусочно-линейный, трапеции точны."
  (let ((points (sort (remove-if-not (lambda (x) (< lo x hi))
                                     (list (muscle-from muscle) (muscle-peak muscle) (muscle-to muscle)))
                      #'<)))
    (loop for (a b) on (append (list lo) points (list hi))
          while b
          sum (* 0.5d0 (- b a) (+ (muscle-profile muscle a) (muscle-profile muscle b))))))

(defun bind-muscles (muscles ring-fractions quadrant-edges)
  "Привязать мышцы к продольным рёбрам.
RING-FRACTIONS — положения колец (доли L) по возрастанию; QUADRANT-EDGES —
функция (квадрант i) → номера продольных рёбер квадранта между кольцами i и i+1.
Вес ребра — средний профиль клетки на участке между кольцами, делённый
поровну между рёбрами квадранта (дизайн, «Мышца → тело»)."
  (let ((muscles (coerce muscles 'simple-vector))
        (offsets (make-ivec (1+ (length muscles))))
        (edges '()) (weights '()) (count 0))
    (loop for m across muscles
          for j from 0
          do (setf (aref offsets j) count)
             (loop for i below (1- (length ring-fractions))
                   for lo = (aref ring-fractions i)
                   for hi = (aref ring-fractions (1+ i))
                   when (and (< lo (muscle-to m)) (> hi (muscle-from m)))
                     do (let ((mean (/ (profile-integral m lo hi) (- hi lo)))
                              (quad-edges (funcall quadrant-edges (muscle-quadrant m) i)))
                          (when (> mean 1d-12)
                            (dolist (e quad-edges)
                              (push e edges)
                              (push (/ mean (length quad-edges)) weights)
                              (incf count))))))
    (setf (aref offsets (length muscles)) count)
    (make-muscle-binding :muscles muscles :offsets offsets
                         :edges (ivec-from (nreverse edges))
                         :weights (dvec-from (nreverse weights)))))

(defun muscle-edges (binding m)
  "Рёбра мышцы номер M: список (ребро . вес)."
  (let ((offsets (muscle-binding-offsets binding)))
    (loop for p from (aref offsets m) below (aref offsets (1+ m))
          collect (cons (aref (muscle-binding-edges binding) p)
                        (aref (muscle-binding-weights binding) p)))))

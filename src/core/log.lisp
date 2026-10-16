;;;; src/core/log.lisp — журнал проекта.
;;;;
;;;; Одна строка на событие: время UTC, уровень, источник, текст. Журнал
;;;; процесса пишется в log/worm.log (каталог log/ не в git); журнал
;;;; прогона в runs/<id>/log.txt появится на этапе 4. Из горячего пути
;;;; журнал не вызывается.

(in-package #:worm.core)

(defvar *log-level* :info
  "Наименьший уровень, который попадает в журнал: :debug, :info, :warn или :error.")

(defvar *log-echo* nil
  "Поток, куда события дублируются, например *standard-output* в REPL; NIL — не дублировать.")

(defvar *log-stream* nil)
(defvar *log-lock* (worm.platform:make-lock "worm log"))

(defparameter +log-levels+ '(:debug :info :warn :error))

(defun log-path ()
  (merge-pathnames "log/worm.log" (project-root)))

(defun ensure-log-stream ()
  (or *log-stream*
      (let ((path (log-path)))
        (ensure-directories-exist path)
        (setf *log-stream* (open path :direction :output :if-exists :append
                                      :if-does-not-exist :create
                                      :external-format :utf-8)))))

(defun close-log ()
  (worm.platform:with-lock (*log-lock*)
    (when *log-stream*
      (close *log-stream*)
      (setf *log-stream* nil))))

(defun utc-timestamp ()
  (multiple-value-bind (s mi h d mo y) (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" y mo d h mi s)))

(defun log-event (level source control &rest args)
  "Записать событие уровня LEVEL от SOURCE (символ или строка) с текстом из CONTROL и ARGS."
  (when (>= (position level +log-levels+) (position *log-level* +log-levels+))
    (let ((line (format nil "~A ~5A ~(~A~): ~?" (utc-timestamp) level source control args)))
      (worm.platform:with-lock (*log-lock*)
        (let ((out (ensure-log-stream)))
          (write-line line out)
          (force-output out))
        (when *log-echo*
          (write-line line *log-echo*)))))
  (values))

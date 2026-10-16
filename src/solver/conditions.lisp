;;;; src/solver/conditions.lisp — численные сбои и перезапуски решателя.
;;;;
;;;; Архитектура, «Ошибки»: numerical-error → numerical-blowup (причина: NaN,
;;;; переполнение, деление на ноль, volume-collapse, singular-matrix) и
;;;; event-storm. Перезапуски: halve-timestep, rollback-to-checkpoint,
;;;; clamp-and-continue, abort-run. Выбирает их либо человек (этап 8), либо
;;;; политика — функция от условия, возвращающая имя перезапуска или NIL.

(in-package #:worm.solver)

(define-condition numerical-blowup (numerical-error)
  ((cause :initarg :cause :reader numerical-blowup-cause
          :documentation ":nan, :overflow, :division-by-zero, :invalid, :singular-matrix, :volume-collapse …")
   (step :initarg :step :initform nil :reader numerical-blowup-step)
   (time :initarg :time :initform nil :reader numerical-blowup-time)
   (h :initarg :h :initform nil :reader numerical-blowup-h)
   (where :initarg :where :initform nil :reader numerical-blowup-where
          :documentation "Подсистема и номер элемента, где обнаружен сбой."))
  (:report (lambda (c stream)
             (format stream "численный сбой ~(~A~) на шаге ~D (t = ~,6F с, h = ~,3E с)~@[, ~A~]"
                     (numerical-blowup-cause c) (numerical-blowup-step c)
                     (numerical-blowup-time c) (numerical-blowup-h c)
                     (numerical-blowup-where c)))))

(define-condition event-storm (numerical-error) ()
  (:documentation "Дребезг: единица переключается чаще заданного предела (этап 10)."))

;;; Политики

(defun never-policy (condition)
  "Политика, которая ничего не выбирает: сбой уходит вызывающему."
  (declare (ignore condition))
  nil)

(defun make-batch-policy (&key (max-halvings 3))
  "Пакетная политика: halve-timestep до MAX-HALVINGS раз за прогон, затем
abort-run; clamp-and-continue — никогда."
  (let ((halvings 0))
    (lambda (condition)
      (typecase condition
        (numerical-blowup
         (if (< halvings max-halvings)
             (progn (incf halvings) 'halve-timestep)
             'abort-run))
        (event-storm (if (< halvings max-halvings)
                         (progn (incf halvings) 'halve-timestep)
                         'abort-run))
        (t nil)))))

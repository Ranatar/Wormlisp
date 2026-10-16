;;;; src/codegen/support.lisp — встраиваемые функции сгенерированного кода.
;;;;
;;;; Значения встроенных примитивов в типизированном виде: сгенерированный
;;;; код зовёт их вместо функций-значений IR, чтобы числа не упаковывались.

(in-package #:worm.codegen)

(declaim (inline %hill %hill-dx))

(defun %hill (x k n)
  (declare (type double-float x k n))
  (if (<= x 0d0)
      0d0
      (let ((r (exp (* n (log (the (double-float (0d0)) (/ x k)))))))
        (declare (type double-float r))
        (/ r (+ 1d0 r)))))

(defun %hill-dx (x k n)
  (declare (type double-float x k n))
  (if (<= x 0d0)
      0d0
      (let ((r (exp (* n (log (the (double-float (0d0)) (/ x k)))))))
        (declare (type double-float r))
        (/ (* n r) (* x (+ 1d0 r) (+ 1d0 r))))))

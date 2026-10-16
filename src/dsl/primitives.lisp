;;;; src/dsl/primitives.lisp — геометрические примитивы языка модели.
;;;;
;;;; Объём отсека и его производные заданы примитивами с написанной вручную
;;;; структурой (архитектура, «Известное узкое место»; решение 21): значение
;;;; — функция Лиспа от 6K координат слотов, производная объёма — градиент
;;;; по каждой координате, производная градиента — гессиан. Значения
;;;; считает worm.topology:compartment-volume-derivatives; число узлов на
;;;; кольце — длина списка аргументов, делённая на 6.

(in-package #:worm.dsl)

(defun points-k (points) (/ (length points) 6))

(worm.ir:define-primitive volume-hess (&rest points)
  :indices (j m)
  :value (lambda (j m &rest points)
           (aref (nth-value 2 (worm.topology:compartment-volume-derivatives
                               points (points-k points) :hessian t))
                 j m))
  :documentation "∂²V/∂x_j∂x_m отсека.")

(worm.ir:define-primitive volume-grad (&rest points)
  :indices (j)
  :value (lambda (j &rest points)
           (aref (nth-value 1 (worm.topology:compartment-volume-derivatives points (points-k points))) j))
  :derivative (loop for m below (length points)
                    collect (worm.ir:prim 'volume-hess points :indices (list j m)))
  :documentation "∂V/∂x_j отсека.")

(worm.ir:define-primitive volume (&rest points)
  :value (lambda (&rest points)
           (values (worm.topology:compartment-volume-derivatives points (points-k points))))
  :derivative (loop for j below (length points)
                    collect (worm.ir:prim 'volume-grad points :indices (list j)))
  :documentation "Объём отсека по координатам его 2K узлов.")

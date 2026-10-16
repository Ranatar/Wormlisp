;;;; src/core/types.lisp — типы массивов горячего пути и мелкие числовые утилиты.
;;;;
;;;; Архитектура, «Производительность и память»: все массивы — simple-array
;;;; с объявленным типом элементов. Состояние и параметры — double-float,
;;;; кадры — single-float, индексы элементов — (unsigned-byte 32).

(in-package #:worm.core)

(deftype index () '(integer 0 #.(1- array-dimension-limit)))
(deftype dvec (&optional (n '*)) `(simple-array double-float (,n)))
(deftype fvec (&optional (n '*)) `(simple-array single-float (,n)))
(deftype ivec (&optional (n '*)) `(simple-array (unsigned-byte 32) (,n)))
(deftype dmat (&optional (n '*) (m '*)) `(simple-array double-float (,n ,m)))

(declaim (inline make-dvec make-fvec make-ivec))

(defun make-dvec (n &optional (initial 0d0))
  (make-array n :element-type 'double-float :initial-element (float initial 0d0)))

(defun make-fvec (n &optional (initial 0f0))
  (make-array n :element-type 'single-float :initial-element (float initial 0f0)))

(defun make-ivec (n &optional (initial 0))
  (make-array n :element-type '(unsigned-byte 32) :initial-element initial))

(defun make-dmat (n m &optional (initial 0d0))
  (make-array (list n m) :element-type 'double-float :initial-element (float initial 0d0)))

(defun dvec-from (sequence)
  "Новый dvec с элементами SEQUENCE, приведёнными к double-float."
  (map 'dvec (lambda (x) (float x 0d0)) sequence))

(defun ivec-from (sequence)
  "Новый ivec с элементами SEQUENCE."
  (map 'ivec #'identity sequence))

(defconstant +pi+ (coerce pi 'double-float))

(declaim (inline sq))
(defun sq (x) (* x x))

(defun approx= (a b &key (rel 1d-12) (abs 0d0))
  "Совпадают ли A и B с относительной точностью REL или абсолютной ABS."
  (<= (abs (- a b)) (max abs (* rel (max (abs a) (abs b))))))

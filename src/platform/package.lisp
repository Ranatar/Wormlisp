;;;; src/platform/package.lisp
;;;;
;;;; Слой 0. Всё, что зависит от SBCL и Linux, собирается здесь, чтобы
;;;; зависимость от реализации была видна в одном месте. Дальше сюда придут
;;;; потоки, атомарные ячейки, ловушки плавающей точки и сохранение образа.

(defpackage #:worm.platform
  (:use #:cl)
  (:export #:implementation-description
           #:dynamic-space-megabytes
           #:processor-count))

(in-package #:worm.platform)

(defun implementation-description ()
  "Строка вида \"SBCL 2.2.9.debian\"."
  (format nil "~A ~A" (lisp-implementation-type) (lisp-implementation-version)))

(defun dynamic-space-megabytes ()
  "Размер динамической памяти процесса в мегабайтах (--dynamic-space-size)."
  (floor (sb-ext:dynamic-space-size) (* 1024 1024)))

(defconstant +sc-nprocessors-onln+ 84
  "Номер _SC_NPROCESSORS_ONLN в glibc для Linux.")

(defun processor-count ()
  "Число доступных процессорных ядер; 1, если узнать не удалось."
  (let ((n (sb-alien:alien-funcall
            (sb-alien:extern-alien "sysconf" (function sb-alien:long sb-alien:int))
            +sc-nprocessors-onln+)))
    (if (plusp n) n 1)))

;;;; src/platform/package.lisp
;;;;
;;;; Слой 0. Всё, что зависит от SBCL и Linux, собирается здесь, чтобы
;;;; зависимость от реализации была видна в одном месте: сведения о процессе,
;;;; блокировки, атомарные ячейки, ловушки плавающей точки, хэши MD5.
;;;; Потоки симуляции, почтовые ящики и сохранение образа придут сюда на
;;;; этапах 8 и 11.

(defpackage #:worm.platform
  (:use #:cl)
  (:export #:implementation-description
           #:dynamic-space-megabytes
           #:processor-count
           ;; блокировки
           #:make-lock #:with-lock
           ;; атомарная ячейка
           #:make-atomic-cell #:atomic-cell-p #:atomic-cell-value
           #:atomic-cell-swap #:atomic-cell-compare-and-swap
           ;; плавающая точка
           #:with-float-traps-masked #:enabled-float-traps
           ;; хэши
           #:md5-hex))

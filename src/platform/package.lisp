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
           ;; потоки, почтовые ящики, семафоры
           #:make-thread #:join-thread #:thread-alive-p #:current-thread #:thread-name
           #:make-mailbox #:mailbox-send #:mailbox-receive #:mailbox-receive-no-hang #:mailbox-count
           #:make-semaphore #:signal-semaphore #:wait-on-semaphore
           ;; файлы и процессы ОС
           #:rename-directory #:process-peak-megabytes #:available-megabytes
           ;; атомарная ячейка
           #:make-atomic-cell #:atomic-cell-p #:atomic-cell-value
           #:atomic-cell-swap #:atomic-cell-compare-and-swap
           ;; плавающая точка
           #:with-float-traps-masked #:enabled-float-traps
           ;; хэши
           #:md5-hex))

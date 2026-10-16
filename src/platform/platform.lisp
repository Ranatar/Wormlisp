;;;; src/platform/platform.lisp

(in-package #:worm.platform)

;;; Сведения о процессе

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

;;; Блокировки. На горячем пути их нет (архитектура, «Потоки»); они нужны
;;; журналу и реестрам, которые трогают из REPL и из потоков сервера.

(defun make-lock (&optional (name "worm lock"))
  (sb-thread:make-mutex :name name))

(defmacro with-lock ((lock) &body body)
  `(sb-thread:with-recursive-lock (,lock) ,@body))

;;; Атомарная ячейка: в неё поток симуляции кладёт снимки, а читатели
;;; берут последнюю ссылку без блокировок.

(defstruct (atomic-cell (:constructor make-atomic-cell (&optional value)))
  (value nil))

(defun atomic-cell-swap (cell new)
  "Записать NEW в ячейку и вернуть прежнее значение."
  (loop (let ((old (atomic-cell-value cell)))
          (when (eq old (sb-ext:compare-and-swap (atomic-cell-value cell) old new))
            (return old)))))

(defun atomic-cell-compare-and-swap (cell old new)
  "Записать NEW, если в ячейке лежит OLD (по EQ); вернуть значение до записи."
  (sb-ext:compare-and-swap (atomic-cell-value cell) old new))

;;; Ловушки плавающей точки. SBCL на x86-64 по умолчанию ловит
;;; переполнение, недопустимую операцию и деление на ноль (архитектура,
;;; «Решатель»). Отключать их можно только явно и на короткий участок.

(defun enabled-float-traps ()
  "Список включённых ловушек плавающей точки, например (:OVERFLOW :INVALID :DIVIDE-BY-ZERO)."
  (getf (sb-int:get-floating-point-modes) :traps))

(defmacro with-float-traps-masked ((&rest traps) &body body)
  "Выполнить BODY с отключёнными ловушками TRAPS (по умолчанию — всеми тремя)."
  `(sb-int:with-float-traps-masked ,(or traps '(:overflow :invalid :divide-by-zero))
     ,@body))

;;; Хэши. MD5 берётся из contrib sb-md5 и служит для хэшей моделей и
;;; наборов параметров в meta.json, а не для защиты.

(defun md5-hex (string)
  "Шестнадцатеричный MD5 строки STRING в кодировке UTF-8."
  (with-output-to-string (out)
    (loop for byte across (sb-md5:md5sum-string string :external-format :utf-8)
          do (format out "~(~2,'0x~)" byte))))

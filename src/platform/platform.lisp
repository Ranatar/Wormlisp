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

;;; Файлы и процессы ОС (пакетные прогоны)

(defun rename-directory (from to)
  "Переименовать каталог FROM в TO одним вызовом rename(2) — атомарно на
одной файловой системе."
  (flet ((native (p) (string-right-trim "/" (sb-ext:native-namestring (uiop:ensure-directory-pathname p)))))
    (multiple-value-bind (ok errno) (sb-unix:unix-rename (native from) (native to))
      (unless ok
        (error "переименование ~A → ~A: ~A" (native from) (native to) (sb-int:strerror errno)))
      to)))

(defun proc-field-kb (path field)
  "Число в килобайтах из строки «FIELD: N kB» файла /proc."
  (with-open-file (in path :if-does-not-exist nil)
    (when in
      (loop for line = (read-line in nil)
            while line
            when (and (> (length line) (length field)) (string= field line :end2 (length field)))
              return (parse-integer line :start (1+ (length field)) :junk-allowed t)))))

(defun process-peak-megabytes (pid)
  "Пиковый размер резидентной памяти процесса PID (VmHWM), МБ, или NIL."
  (let ((kb (proc-field-kb (format nil "/proc/~D/status" pid) "VmHWM")))
    (and kb (/ kb 1024.0))))

(defun available-megabytes ()
  "Доступная память системы (MemAvailable), МБ."
  (let ((kb (proc-field-kb "/proc/meminfo" "MemAvailable")))
    (if kb (/ kb 1024.0) 0.0)))

;;; Блокировки. На горячем пути их нет (архитектура, «Потоки»); они нужны
;;; журналу и реестрам, которые трогают из REPL и из потоков сервера.

(defun make-lock (&optional (name "worm lock"))
  (sb-thread:make-mutex :name name))

(defmacro with-lock ((lock) &body body)
  `(sb-thread:with-recursive-lock (,lock) ,@body))

;;; Потоки, почтовые ящики и семафоры (архитектура, «Потоки и интерактивная
;;; работа»): поток симуляции читает команды из почтового ящика, синхронные
;;; функции REPL ждут ответа на семафоре «обещания».

(defun make-thread (function &key (name "worm thread"))
  (sb-thread:make-thread function :name name))

(defun join-thread (thread &key timeout)
  "Дождаться завершения THREAD; NIL, если не дождались за TIMEOUT секунд."
  (if timeout
      (multiple-value-bind (value status) (sb-thread:join-thread thread :default nil :timeout timeout)
        (declare (ignore status))
        (values value (not (sb-thread:thread-alive-p thread))))
      (values (sb-thread:join-thread thread :default nil) t)))

(defun thread-alive-p (thread) (and thread (sb-thread:thread-alive-p thread)))
(defun current-thread () sb-thread:*current-thread*)
(defun thread-name (thread) (sb-thread:thread-name thread))

(defun make-mailbox (&optional (name "worm mailbox"))
  (sb-concurrency:make-mailbox :name name))

(defun mailbox-send (mailbox message)
  (sb-concurrency:send-message mailbox message))

(defun mailbox-receive (mailbox &key timeout)
  "Следующее сообщение; ждёт, пока оно не придёт (или TIMEOUT секунд).
Вторым значением — T, если сообщение получено."
  (if timeout
      (sb-concurrency:receive-message mailbox :timeout timeout)
      (values (sb-concurrency:receive-message mailbox) t)))

(defun mailbox-receive-no-hang (mailbox)
  "Сообщение, если оно уже есть: (values сообщение получено-p)."
  (sb-concurrency:receive-message-no-hang mailbox))

(defun mailbox-count (mailbox) (sb-concurrency:mailbox-count mailbox))

(defun make-semaphore (&optional (name "worm semaphore"))
  (sb-thread:make-semaphore :name name :count 0))

(defun signal-semaphore (semaphore) (sb-thread:signal-semaphore semaphore))

(defun wait-on-semaphore (semaphore &key timeout)
  "T, если дождались; NIL по истечении TIMEOUT секунд."
  (and (sb-thread:wait-on-semaphore semaphore :timeout timeout) t))

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

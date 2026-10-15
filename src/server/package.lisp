;;;; src/server/package.lisp
;;;;
;;;; Слой 6. HTTP-сервер процесса REPL. На этапе 0 он отдаёт только
;;;; проверочную страницу; раздача просмотрщика, прогонов и WebSocket
;;;; придут на этапах 4 и 9.

(defpackage #:worm.server
  (:use #:cl)
  (:export #:serve
           #:stop-serving
           #:serving-p
           #:*default-port*))

;;;; src/server/package.lisp
;;;;
;;;; Слой 6. HTTP-сервер процесса REPL: просмотрщик, прогоны, WebSocket
;;;; /live (архитектура, «Сервер живого просмотра и протокол»).

(defpackage #:worm.server
  (:use #:cl)
  (:export #:serve
           #:stop-serving
           #:serving-p
           #:*default-port*
           #:*read-only*
           #:live-clients))

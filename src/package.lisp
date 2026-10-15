;;;; src/package.lisp
;;;;
;;;; Фасадный пакет WORM — то, чем пользуются из REPL. Пакет WORM-USER —
;;;; рабочий пакет терминального REPL (scripts/repl.sh переходит в него).

(defpackage #:worm
  (:use #:cl)
  (:import-from #:worm.server #:serve #:stop-serving #:serving-p)
  (:export #:serve
           #:stop-serving
           #:serving-p
           #:about))

(in-package #:worm)

(defun about (&optional (stream *standard-output*))
  "Напечатать сведения о процессе: реализация, ревизия, память, ядра, сервер."
  (let ((rev (worm.core:git-revision)))
    (format stream "~&Wormlisp ~A~%  ~A~%  ревизия ~A~%  память ~D МБ, ядер ~D~%  сервер ~A~%"
            (asdf:component-version (asdf:find-system "worm"))
            (worm.platform:implementation-description)
            (or rev "неизвестна")
            (worm.platform:dynamic-space-megabytes)
            (worm.platform:processor-count)
            (if (serving-p) "запущен" "не запущен")))
  (values))

(defpackage #:worm-user
  (:use #:cl #:worm))

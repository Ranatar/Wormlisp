;;;; src/package.lisp
;;;;
;;;; Фасадный пакет WORM — то, чем пользуются из REPL. Пакет WORM-USER —
;;;; рабочий пакет терминального REPL (scripts/repl.sh переходит в него).

(defpackage #:worm
  (:use #:cl)
  (:import-from #:worm.server #:serve #:stop-serving #:serving-p)
  (:import-from #:worm.runtime
   #:build-model #:start-run #:stop-run #:wait-run #:list-runs #:*current-run* #:pause #:resume #:step-run #:call-in-run #:set-param #:param #:load-params #:snapshot #:state-ref #:save-checkpoint #:rollback #:record #:record-to #:problem #:choose #:debug-problem #:rebuild #:benchmark-step #:profile-run)
  (:export #:serve
           #:stop-serving
           #:serving-p
           #:about
           #:build-model #:start-run #:stop-run #:wait-run #:list-runs #:*current-run* #:pause #:resume #:step-run #:call-in-run #:set-param #:param #:load-params #:snapshot #:state-ref #:save-checkpoint #:rollback #:record #:record-to #:problem #:choose #:debug-problem #:rebuild #:benchmark-step #:profile-run))

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

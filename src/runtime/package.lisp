;;;; src/runtime/package.lisp
;;;;
;;;; Слой 5. Объект run, поток симуляции, команды, снимки, API для REPL —
;;;; этап 8. С этапа 7 здесь замеры шага: benchmark-step и profile-run
;;;; (архитектура, «Производительность и память», «Как мерить»).

(defpackage #:worm.runtime
  (:use #:cl #:worm.core)
  (:export
   #:make-model-integrator #:benchmark-step #:profile-run #:write-benchmark))

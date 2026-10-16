;;;; src/runtime/package.lisp
;;;;
;;;; Слой 5. Объект run, поток симуляции, команды, снимки, API для REPL
;;;; (архитектура, «Потоки и интерактивная работа»); замеры шага
;;;; benchmark-step и profile-run («Производительность и память»).

(defpackage #:worm.runtime
  (:use #:cl #:worm.core)
  (:export
   ;; замеры (этап 7)
   #:make-model-integrator #:benchmark-step #:profile-run #:write-benchmark
   ;; прогон
   #:run #:run-p #:run-id #:run-model #:run-integrator #:run-status #:run-directory #:run-policy
   #:*current-run* #:list-runs #:build-model #:start-run #:stop-run #:wait-run
   #:pause #:resume #:step-run #:run-status-of #:call-in-run #:flush-recording
   #:set-param #:param #:load-params
   #:snapshot #:snapshot-p #:snapshot-step #:snapshot-time #:snapshot-h #:snapshot-status
   #:snapshot-coords #:snapshot-activations #:snapshot-curvature #:snapshot-rings #:snapshot-ring-nodes
   #:state-ref #:save-checkpoint #:rollback #:record #:record-to
   #:problem #:problem-p #:problem-cause #:problem-step #:problem-time #:problem-h #:problem-where
   #:problem-restarts #:problem-report #:problem-condition #:run-problem #:choose #:debug-problem
   #:rebuild
   ;; для сервера
   #:post-command #:find-run #:prepare-param-change #:run-parameters #:run-description
   #:run-param-change-log #:*runs* #:abort-run-with
   #:run-error #:run-timeout))

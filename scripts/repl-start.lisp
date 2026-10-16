;;;; scripts/repl-start.lisp — то, что scripts/repl.sh выполняет при старте REPL.

(asdf:load-system "worm")
(let ((port (let ((env (uiop:getenv "WORM_PORT")))
              (if (and env (plusp (length env))) (parse-integer env) 8740))))
  (worm:serve :port port))
(worm:about)
(format t "~&Прогон:  (start-run 'worm.components:worm-3d :params \"agar-a-v1\" :dt 1d-3 :record t)
         (pause) (resume) (step-run 100) (set-param :tau-ca 60) (param :tau-ca)
         (state-ref :ring 10 :node 3 :axis :x) (save-checkpoint) (rollback 0)
         (problem) (choose 'halve-timestep) (rebuild 'worm.components:worm-3d) (stop-run)
Отключиться, не останавливая REPL и прогоны: Ctrl-b, затем d. Вернуться: scripts/repl.sh~%")

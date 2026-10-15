;;;; scripts/repl-start.lisp — то, что scripts/repl.sh выполняет при старте REPL.

(asdf:load-system "worm")
(let ((port (let ((env (uiop:getenv "WORM_PORT")))
              (if (and env (plusp (length env))) (parse-integer env) 8740))))
  (worm:serve :port port))
(worm:about)
(format t "~&Отключиться, не останавливая REPL: Ctrl-b, затем d. Вернуться: scripts/repl.sh~%")

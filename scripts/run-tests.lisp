;;;; scripts/run-tests.lisp — прогон тестов для make test.
;;;; Код возврата 0 — всё прошло, 1 — есть падения.

(asdf:load-system "worm/tests")
(uiop:quit (if (uiop:symbol-call :worm.tests :run-all) 0 1))

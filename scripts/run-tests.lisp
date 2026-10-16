;;;; scripts/run-tests.lisp — прогон тестов для make test.
;;;; Код возврата 0 — всё прошло, 1 — есть падения. WORM_FULL=1 — все тесты (make test-full).

(asdf:load-system "worm/tests")
(uiop:quit (if (uiop:symbol-call :worm.tests (if (uiop:getenv "WORM_FULL") :run-full :run-all)) 0 1))

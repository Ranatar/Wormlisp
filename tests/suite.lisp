;;;; tests/suite.lisp — корневой набор тестов и точка входа для make test.

(in-package #:worm.tests)

(def-suite worm :description "Все тесты Wormlisp.")

(defun run-all ()
  "Прогнать все тесты; T, если всё прошло."
  (run! 'worm))

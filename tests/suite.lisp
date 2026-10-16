;;;; tests/suite.lisp — корневой набор тестов и точка входа для make test.

(in-package #:worm.tests)

(def-suite worm :description "Все тесты Wormlisp.")

(defun run-all ()
  "Быстрые тесты (make test); T, если всё прошло."
  (run! 'worm))

(defun run-full ()
  "Все тесты, включая физику тела и среды (make test-full); T, если всё прошло."
  (let ((quick (run! 'worm))
        (physics (run! 'physics)))
    (and quick physics)))

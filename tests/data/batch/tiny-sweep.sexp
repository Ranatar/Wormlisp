;;;; Проверочная развёртка тестов: три задания и одно с недопустимым значением.
(sweep tiny-sweep
  :base #p"tests/data/batch/tiny.sexp"
  :vary ((sr-scale :list (1 1.4 2 -5)))
  :design :grid)

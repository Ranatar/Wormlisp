;;;; Задание, которое не укладывается в предел времени.
(sweep slow-sweep
  :base #p"tests/data/batch/tiny.sexp"
  :vary ((sr-scale :list (1.4)))
  :duration 600
  :timeout 4)

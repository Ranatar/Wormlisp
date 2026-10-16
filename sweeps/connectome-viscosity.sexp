;;;; Главный тест на третьем уровне (этап 12): вязкость воды растёт от 1 до
;;;; 10⁴ при неизменных параметрах сети.
(sweep connectome-viscosity
  :base #p"configs/connectome-water.sexp"
  :vary ((viscosity :list (1 10 100 1000 3000 10000) :multiply t :params (c-t c-n)))
  :duration 10
  :timeout 7200)

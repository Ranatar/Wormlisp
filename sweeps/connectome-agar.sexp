;;;; Агар для главного теста третьего уровня (этап 12): базовые параметры.
(sweep connectome-agar
  :base #p"configs/connectome-agar.sexp"
  :vary ((command-current :list (0)))
  :duration 12
  :timeout 7200)

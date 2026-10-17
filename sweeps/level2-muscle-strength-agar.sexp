;;;; Второй уровень (бинарная цепь) с силой мышц, при которой первый уровень
;;;; гнётся на агаре как живой червь (этап 14): κ·L ≈ 8 при F_max ≈ 48 мкН.
(sweep level2-muscle-strength-agar
  :base #p"configs/proprio-agar.sexp"
  :vary ((f-max-mid :list (16.5 33 48)))
  :design :grid
  :duration 16
  :timeout 3600)

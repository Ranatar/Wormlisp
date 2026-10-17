;;;; Пара к level2-muscle-strength-agar: та же сила мышц в воде (этап 14).
(sweep level2-muscle-strength-water
  :base #p"configs/proprio-water.sexp"
  :vary ((f-max-mid :list (16.5 33 48)))
  :design :grid
  :timeout 3600)

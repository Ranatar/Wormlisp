;;;; Уровень 1 на агаре с волной живого червя (этап 14): какая сила мышц и
;;;; какое сопротивление агара дают амплитуду кривизны трека N2 (κ·L ≈ 8) и
;;;; держит ли тело заданную длину волны 0,62 L.
(sweep rhythm-agar-amplitude
  :base #p"configs/rhythm-agar.sexp"
  :vary ((f-max-mid :geometric 16.5 66 :steps 3)
         (c-n :list (128 280)))
  :design :grid
  :timeout 3600)

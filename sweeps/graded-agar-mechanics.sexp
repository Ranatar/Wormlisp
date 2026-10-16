;;;; Механическое время тела на агаре (план, этап 11): сопротивление C_n,
;;;; изгибная жёсткость EI и сила мышц F_max при базовом контроллере
;;;; проводимостной версии.
(sweep graded-agar-mechanics
  :base #p"configs/graded-agar.sexp"
  :vary ((c-n :geometric 64 512 :steps 2)
         (bending-ei :geometric 0.095 0.28 :steps 2)
         (f-max-mid :geometric 8 33 :steps 2))
  :design (:latin-hypercube :samples 12 :seed 5)
  :duration 12
  :timeout 3600)

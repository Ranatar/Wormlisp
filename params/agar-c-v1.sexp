;;;; params/agar-c-v1.sexp — агар 1,7%, вариант В (по Rabets et al. 2014)
;;;; Значения, единицы, степени достоверности и источники — по
;;;; docs/parameters.md; файл и документ правятся вместе. Степени:
;;;; :measured — И, :estimate — О, :fit — П, :numeric — Ч.

(params agar-c-v1 :extends agar-a-v1
  ;; линейное приближение Rabets 2014 при 100 мкм/с, агар 1,7%
  (c-t   46      :uN*s/mm^2 :level :measured :source "Rabets 2014")
  (c-n   280     :uN*s/mm^2 :level :measured :source "Rabets 2014"))

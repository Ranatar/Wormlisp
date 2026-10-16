;;;; params/agar-a-v1.sexp — агар, вариант А (по Boyle et al. 2012)
;;;; Значения, единицы, степени достоверности и источники — по
;;;; docs/parameters.md; файл и документ правятся вместе. Степени:
;;;; :measured — И, :estimate — О, :fit — П, :numeric — Ч.

(params agar-a-v1 :extends base-v1
  (c-t   3.2     :uN*s/mm^2 :level :estimate :source "Niebur-Erdos 1991, fit to ~1 mm/s")
  (c-n   128     :uN*s/mm^2 :level :estimate :source "Boyle 2012, K=40 after Berri 2009")
  (contact t)
  (meniscus-force 400 :uN/mm :level :estimate :bounds (350 900) :source "Rabets 2014, agar 1.7%"))

;;;; params/water-v1.sexp — вода, μ = 1 мПа·с
;;;; Значения, единицы, степени достоверности и источники — по
;;;; docs/parameters.md; файл и документ правятся вместе. Степени:
;;;; :measured — И, :estimate — О, :fit — П, :numeric — Ч.

(params water-v1 :extends base-v1
  (c-t   3.3e-3  :uN*s/mm^2 :level :estimate :source "Lighthill 1976 sec. 3.3, R=40um")
  (c-n   5.2e-3  :uN*s/mm^2 :level :estimate :source "Lighthill 1976 sec. 3.3, R=40um")
  (contact nil))

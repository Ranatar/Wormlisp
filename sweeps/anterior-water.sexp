;;;; Поле вперёд с головным генератором на втором уровне (этап 12а): поиск
;;;; плавания в воде.
(sweep anterior-water
  :base #p"configs/proprio-water-anterior.sexp"
  :vary ((rf-length :linear 0.1 0.35 :steps 2)
         (head-rf-length :linear 0.04 0.15 :steps 2)
         (head-adapt-gain :geometric 0.3 1.5 :steps 2)
         (head-adapt-tau :geometric 150 1000 :steps 2)
         (sr-scale :geometric 1.5 8 :steps 2)
         (sr-filter :geometric 30 200 :steps 2))
  :design (:latin-hypercube :samples 20 :seed 37)
  :duration 6
  :timeout 3600)

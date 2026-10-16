;;;; Поиск настройки второго уровня (план, этап 11): та же выборка
;;;; латинского гиперкуба в воде, вязкой воде и на агаре.
(sweep level2-water
  :base #p"configs/proprio-water.sexp"
  :vary ((sr-filter :geometric 50 400 :steps 2)
         (sr-scale :geometric 0.8 2.5 :steps 2)
         (rf-length :linear 0.2 0.5 :steps 2)
         (tau-ca :geometric 80 400 :steps 2))
  :design (:latin-hypercube :samples 12 :seed 7)
  :duration 6
  :timeout 3600)

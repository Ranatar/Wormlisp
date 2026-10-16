;;;; Поиск настройки проводимостной версии B-нейрона (план, этап 11): одна и
;;;; та же выборка латинского гиперкуба в воде, вязкой воде (×1000) и на агаре.
(sweep graded-viscous
  :base #p"configs/graded-viscous.sexp"
  :vary ((gb-c :geometric 5 50 :steps 2)
         (gb-sr-current :geometric 30 300 :steps 2)
         (sr-filter :geometric 20 300 :steps 2)
         (rf-length :linear 0.2 0.5 :steps 2))
  :design (:latin-hypercube :samples 12 :seed 11)
  :duration 10
  :timeout 3600)

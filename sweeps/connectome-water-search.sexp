;;;; Поиск плавания третьего уровня в воде (этап 12): выборка вокруг
;;;; настройки, найденной на агаре.
(sweep connectome-water-search
  :base #p"configs/connectome-water.sexp"
  :vary ((gap-motor-scale :geometric 0.02 0.3 :steps 2)
         (neuron-nmj-gain :geometric 4 15 :steps 2)
         (neuron-rest-activity :geometric 0.04 0.3 :steps 2)
         (neuron-sr-conductance :geometric 10000 150000 :steps 2)
         (nmj-head-scale :linear 0 0.5 :steps 2)
         (command-current :linear 0 40 :steps 2))
  :design (:latin-hypercube :samples 12 :seed 21)
  :duration 8
  :timeout 7200)

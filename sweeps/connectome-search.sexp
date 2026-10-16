;;;; Поиск настройки третьего уровня на агаре (этап 12а): масштаб химических
;;;; синапсов держит пороги ниже потенциала реверсии возбуждения.
(sweep connectome-search
  :base #p"configs/connectome-agar.sexp"
  :vary ((chemical-scale :geometric 0.01 0.2 :steps 2)
         (neuron-rest-activity :geometric 0.05 0.3 :steps 2)
         (neuron-sr-conductance :geometric 1000 100000 :steps 2)
         (neuron-nmj-gain :geometric 2 15 :steps 2)
         (gap-motor-scale :geometric 0.02 0.5 :steps 2)
         (gap-command-scale :geometric 0.2 1 :steps 2)
         (nmj-head-scale :linear 0 0.5 :steps 2))
  :design (:latin-hypercube :samples 24 :seed 47)
  :duration 8
  :timeout 7200)

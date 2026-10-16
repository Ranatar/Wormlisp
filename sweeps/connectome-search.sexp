;;;; Поиск настройки третьего уровня на агаре (этап 12): выборка латинского
;;;; гиперкуба по масштабам классов связей, нервно-мышечному синапсу,
;;;; активности синапсов в покое, каналу растяжения B-нейронов, команде
;;;; вперёд и сглаживанию сигнала рецептора.
(sweep connectome-search
  :base #p"configs/connectome-agar.sexp"
  :vary ((gap-command-scale :geometric 0.4 1 :steps 2)
         (gap-motor-scale :geometric 0.01 0.1 :steps 2)
         (neuron-nmj-gain :geometric 4 12 :steps 2)
         (nmj-head-scale :linear 0 0.4 :steps 2)
         (neuron-rest-activity :geometric 0.08 0.3 :steps 2)
         (neuron-sr-conductance :geometric 5000 100000 :steps 2)
         (command-current :linear 0 30 :steps 2)
         (sr-filter :geometric 50 300 :steps 2))
  :design (:latin-hypercube :samples 24 :seed 17)
  :duration 8
  :timeout 3600)

;;;; Пробная развёртка третьего уровня (этап 12): активность синапсов в
;;;; покое, усиление нервно-мышечного синапса и ток рецептора B-нейронов на агаре.
(sweep connectome-probe
  :base #p"configs/connectome-agar.sexp"
  :vary ((neuron-rest-activity :list (0.05 0.2))
         (neuron-nmj-gain :list (10 30))
         (neuron-sr-current :list (1000 3000)))
  :duration 8
  :timeout 3600)

;;;; Третий уровень с рецептивным полем вперёд (Wen 2012) на агаре, этап 12.
(sweep connectome-anterior-agar
  :base #p"configs/connectome-agar-anterior.sexp"
  :vary ((neuron-sr-conductance :list (10000 40000)))
  :duration 8
  :timeout 7200)

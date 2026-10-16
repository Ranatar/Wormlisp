;;;; Сходимость третьего уровня по шагу (этап 12): h = 10⁻³, 5·10⁻⁴ и
;;;; 2,5·10⁻⁴ при R = 10 на агаре.
(sweep connectome-timestep
  :base #p"configs/connectome-agar.sexp"
  :vary ((dt :list (1d-3 5d-4 2.5d-4)))
  :duration 8
  :timeout 7200)

;;;; Сходимость третьего уровня по R (этап 12): пересборка нервной матрицы
;;;; раз в 1, 5, 10 и 20 шагов при h = 10⁻³ на агаре.
(sweep connectome-convergence
  :base #p"configs/connectome-agar.sexp"
  :vary ((neural-every :list (1 5 10 20)))
  :duration 8
  :timeout 7200)

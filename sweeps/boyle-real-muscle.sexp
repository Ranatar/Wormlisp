;;;; Второй уровень на реалистичной механике: поиск мышцы между быстрой
;;;; (boyle-real: вода как у живого червя, на агаре изгиб мал) и медленной
;;;; (boyle-real-slow: агар как у живого червя, в воде волна назад) (план,
;;;; этап 15). Концы — вода и агар по Rabets.
(sweep boyle-real-muscle
  :base #p"configs/boyle-compare.sexp"
  :vary ((medium :list (0 1)
                 :interpolate ((c-t 3.3e-3 46) (c-n 5.2e-3 280)))
         (bending-ei :list (0.095))
         (f-max-mid :list (11 16.3))
         (tau-ca :list (150 200))
         (v-max :list (5 15)))
  :design :grid
  :timeout 3600)

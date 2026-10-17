;;;; Реалистичная механика (как boyle-real) с полем рецептора 0,5 L, как у
;;;; Бойла, при двух усилениях рецептора (план, этап 15): растёт ли изгиб на
;;;; агаре, когда поле длиннее полуволны.
(sweep boyle-real-field
  :base #p"configs/boyle-compare.sexp"
  :vary ((medium :list (0 0.01 0.1 1)
                 :interpolate ((c-t 3.3e-3 46) (c-n 5.2e-3 280)))
         (bending-ei :list (0.095))
         (f-max-mid :list (7.1))
         (rf-length :list (0.5))
         (sr-scale :list (1 1.4)))
  :design :grid
  :timeout 3600)

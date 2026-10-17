;;;; Второй уровень на реалистичной механике с медленной мышцей (план, этап
;;;; 15): EI 0,095, мышцы 16,3 мкН, τ_Ca 250 мс, v_max 5 1/с — на первом
;;;; уровне это даёт κ·L 3,0 в воде и 6,7 на агаре (bandwidth-real). Среда от
;;;; воды к агару по Rabets; сглаживание рецептора 200 и 50 мс.
(sweep boyle-real-slow
  :base #p"configs/boyle-compare.sexp"
  :vary ((medium :list (0 0.001 0.01 0.1 1)
                 :interpolate ((c-t 3.3e-3 46) (c-n 5.2e-3 280)))
         (bending-ei :list (0.095))
         (f-max-mid :list (16.3))
         (tau-ca :list (250))
         (v-max :list (5))
         (sr-filter :list (200 50)))
  :design :grid
  :timeout 3600)

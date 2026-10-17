;;;; Сверка второго уровня с исходной моделью Бойла (план, этап 15): без сглаживания сигнала рецептора (τ_sr = 1 мс).
(sweep boyle-filter
  :base #p"configs/boyle-compare.sexp"
  :vary ((medium :list (0 0.001 0.01 0.1 1)
                 :interpolate ((c-t 3.3e-3 3.2) (c-n 5.2e-3 128)))
         (sr-filter :list (1)))
  :design :grid
  :timeout 3600)

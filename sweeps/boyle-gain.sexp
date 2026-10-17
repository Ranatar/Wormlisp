;;;; Сверка второго уровня с исходной моделью Бойла (план, этап 15): усиление рецептора как в коде Бойла: 0,65·(0,4 + 0,08·i)/2 ≈ 0,46·(0,224 + 0,056·n).
(sweep boyle-gain
  :base #p"configs/boyle-compare.sexp"
  :vary ((medium :list (0 0.001 0.01 0.1 1)
                 :interpolate ((c-t 3.3e-3 3.2) (c-n 5.2e-3 128)))
         (sr-scale :list (0.46)))
  :design :grid
  :timeout 3600)

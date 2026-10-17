;;;; Сверка второго уровня с исходной моделью Бойла (план, этап 15): мышцы, гнущие тело как у Бойла на агаре (F_max ≈ 48 мкН, этап 14).
(sweep boyle-muscle
  :base #p"configs/boyle-compare.sexp"
  :vary ((medium :list (0 0.001 0.01 0.1 1)
                 :interpolate ((c-t 3.3e-3 3.2) (c-n 5.2e-3 128)))
         (f-max-mid :list (48)))
  :design :grid
  :timeout 3600)

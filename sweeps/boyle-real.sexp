;;;; Второй уровень на реалистичной механике (план, этап 15): EI по Fang-Yen
;;;; (0,095), мышцы в той же пропорции (7,1 мкН), среда от воды к агару по
;;;; Rabets 2014 (C_t 46, C_n 280); нервная часть — базовый набор.
(sweep boyle-real
  :base #p"configs/boyle-compare.sexp"
  :vary ((medium :list (0 0.001 0.01 0.1 1)
                 :interpolate ((c-t 3.3e-3 46) (c-n 5.2e-3 280)))
         (bending-ei :list (0.095))
         (f-max-mid :list (7.1)))
  :design :grid
  :timeout 3600)

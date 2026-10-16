;;;; Главный тест этапа 10 по второму уровню (план, этап 11): вязкость воды
;;;; ×μ (C_t и C_n вместе) против постоянной кальция и сглаживания рецептора.
(sweep proprio-viscosity
  :base #p"configs/proprio-water.sexp"
  :vary ((viscosity :geometric 1 10000 :steps 5 :multiply t :params (c-t c-n))
         (sr-filter :list (100 200))
         (tau-ca :list (100 200)))
  :design :grid
  :duration 10
  :timeout 3600)

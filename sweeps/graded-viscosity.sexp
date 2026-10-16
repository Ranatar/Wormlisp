;;;; Главный тест для проводимостной версии B-нейрона (этап 11): вязкость
;;;; воды растёт от 1 до 10⁴ при неизменных параметрах контроллера.
(sweep graded-viscosity
  :base #p"configs/graded-water.sexp"
  :vary ((viscosity :list (1 10 100 1000 3000 10000) :multiply t :params (c-t c-n)))
  :duration 10
  :timeout 3600)

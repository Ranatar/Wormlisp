;;;; Мышца в частотной характеристике тела в воде (план, этап 15): вязкость
;;;; активной мышцы (v_max 5 вместо 30 1/с — у мышцы Бойла активный элемент с
;;;; постоянной 125 мс) и постоянная кальция 200 мс вместо 100.
(sweep forced-muscle-water
  :base #p"configs/rhythm-water.sexp"
  :vary ((gen-frequency :list (0.5 2 4))
         (gen-wavelength :list (1.5))
         (variant :list (0 1)
                  :interpolate ((v-max 5 30) (tau-ca 100 200))))
  :design :grid
  :duration 6
  :timeout 3600)

;;;; Внутренняя вязкость тела в воде (план, этап 15): частотная
;;;; характеристика при τ кутикулы 25 мс (у тела Бойла пассивные элементы
;;;; 25 мс, активные 125 мс) и при EI по Fang-Yen с мышцами в той же пропорции.
(sweep forced-damping-water
  :base #p"configs/rhythm-water.sexp"
  :vary ((gen-frequency :list (0.5 2 4))
         (gen-wavelength :list (1.5))
         (variant :list (0 1)
                  :interpolate ((tau-cuticle 25 5) (bending-ei 0.22 0.095) (f-max-mid 16.5 7.1))))
  :design :grid
  :duration 6
  :timeout 3600)

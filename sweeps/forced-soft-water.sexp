;;;; Частотная характеристика мягкого тела в воде (план, этап 15): изгибная
;;;; жёсткость как у тела Бойла (≈ 0,035 мкН·мм² против 0,22 у нас) и сила
;;;; мышц в той же пропорции (16,5 × 0,035/0,22 ≈ 2,6 мкН).
(sweep forced-soft-water
  :base #p"configs/rhythm-water.sexp"
  :vary ((gen-frequency :list (0.5 1 2 4))
         (gen-wavelength :list (1.5))
         (bending-ei :list (0.035))
         (f-max-mid :list (2.6)))
  :design :grid
  :duration 6
  :timeout 3600)

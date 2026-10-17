;;;; Тело против сопротивления на агаре (план, этап 15): заданная волна
;;;; λ = 1,5 L. Сопротивление ×6 к агару Бойла — эквивалент его мягкого тела
;;;; (EI ≈ 0,035 против 0,22, мышцы в той же пропорции); и реалистичное
;;;; сочетание: EI по Fang-Yen (0,095), мышцы в той же пропорции, агар по
;;;; Rabets 2014 (C_n 280, C_t 46).
(sweep forced-drag-agar
  :base #p"configs/rhythm-water.sexp"
  :vary ((gen-frequency :list (0.3 0.5))
         (gen-wavelength :list (1.5))
         (variant :list (0 1)
                  :interpolate ((c-t 19.2 46) (c-n 768 280) (bending-ei 0.22 0.095) (f-max-mid 16.5 7.1))))
  :design :grid
  :duration 12
  :timeout 3600)

;;;; Пара к forced-soft-water на агаре Бойла без контакта (план, этап 15).
(sweep forced-soft-agar
  :base #p"configs/rhythm-water.sexp"
  :vary ((gen-frequency :list (0.3 0.5 1))
         (gen-wavelength :list (1.5))
         (bending-ei :list (0.035))
         (f-max-mid :list (2.6))
         (c-t :list (3.2))
         (c-n :list (128)))
  :design :grid
  :duration 12
  :timeout 3600)

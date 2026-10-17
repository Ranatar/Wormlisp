;;;; Частотная характеристика тела на агаре (план, этап 15): пара к
;;;; forced-water; сопротивление агара Бойла (C_t 3,2, C_n 128) без контакта
;;;; и мениска, как у его модели.
(sweep forced-agar
  :base #p"configs/rhythm-water.sexp"
  :vary ((gen-frequency :list (0.3 0.5 1 2))
         (gen-wavelength :list (1.5))
         (f-max-mid :list (16.5 48))
         (c-t :list (3.2))
         (c-n :list (128)))
  :design :grid
  :duration 12
  :timeout 3600)

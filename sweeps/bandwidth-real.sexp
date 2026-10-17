;;;; Амплитуда как полоса пропускания тела (план, этап 15): уровень 1 на
;;;; реалистичной механике (EI 0,095, агар по Rabets) с волной живого червя —
;;;; вода 1,76 Гц и 1,54 L, агар 0,32 Гц и 0,62 L. Мышцы в 2,3 раза сильнее,
;;;; чтобы медленный изгиб доходил до κ·L живого червя на агаре (8,2), а
;;;; быстрый в воде гасили кальций (τ_Ca) и вязкость мышцы (v_max).
(sweep bandwidth-real
  :base #p"configs/rhythm-water.sexp"
  :vary ((medium :list (0 1)
                 :interpolate ((c-t 3.3e-3 46) (c-n 5.2e-3 280) (gen-frequency 1.76 0.32) (gen-wavelength 1.54 0.62)))
         (bending-ei :list (0.095))
         (f-max-mid :list (16.3))
         (tau-ca :list (100 250))
         (v-max :list (30 5)))
  :design :grid
  :duration 12
  :timeout 3600)

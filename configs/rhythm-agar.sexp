;;;; Уровень 1 на агаре с волной живого червя (этап 14): частота и длина
;;;; волны — медианы трека N2 (reference/schafer-n2-agar), 50 колец.
(config rhythm-agar
  :model worm-3d
  :params "agar-a-v1"
  :dt 1d-3
  :duration 20
  :set ((gen-frequency 0.32) (gen-wavelength 0.62))
  :record (:frames-every 0.04)
  :metrics-from 6
  :timeout 3600)

;;;; Проприоцептивная цепь в вязкой воде: C_t и C_n ×1000 (главный тест этапа 10).
(config proprio-viscous
  :model worm-3d-proprio
  :params "water-v1"
  :dt 5d-4
  :duration 10
  :set ((c-t 3.3 :uN*s/mm^2) (c-n 5.2 :uN*s/mm^2))
  :record (:frames-every 0.01)
  :metrics-from 4
  :timeout 3600)

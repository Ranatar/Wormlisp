;;;; Проприоцептивная цепь в воде (этап 10): базовые параметры, 50 колец.
(config proprio-water
  :model worm-3d-proprio
  :params "water-v1"
  :dt 5d-4
  :duration 6
  :record (:frames-every 0.01)
  :metrics-from 2
  :timeout 1800)

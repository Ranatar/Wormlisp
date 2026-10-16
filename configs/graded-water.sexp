;;;; Проводимостная проприоцептивная цепь (этап 11) в воде (этап 10): базовые параметры, 50 колец.
(config graded-water
  :model worm-3d-proprio-graded
  :params "water-v1"
  :dt 5d-4
  :duration 10
  :record (:frames-every 0.01)
  :metrics-from 4
  :timeout 1800)

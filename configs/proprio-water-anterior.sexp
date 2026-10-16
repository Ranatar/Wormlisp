;;;; Проприоцептивная цепь с полем вперёд и головным генератором (этап 12а) в воде (этап 10): базовые параметры, 50 колец.
(config proprio-water-anterior
  :model worm-3d-proprio
  :params "water-v1"
  :dt 5d-4
  :duration 6
  :set ((rf-direction anterior) (avb-ventral 0.675) (head-bias 0.5))
  :record (:frames-every 0.01)
  :metrics-from 2
  :timeout 1800)

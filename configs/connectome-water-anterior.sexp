;;;; Сеть коннектома (этап 12), поле вперёд (Wen 2012) в воде (этап 10): базовые параметры, 50 колец.
(config connectome-water-anterior
  :model worm-3d-connectome
  :params "water-v1"
  :dt 5d-4
  :duration 10
  :set ((rf-direction anterior))
  :record (:frames-every 0.01)
  :metrics-from 2
  :timeout 1800)

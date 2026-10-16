;;;; Сеть коннектома (этап 12), поле вперёд (Wen 2012) на агаре (этап 10): базовые параметры, 50 колец.
(config connectome-agar-anterior
  :model worm-3d-connectome
  :params "agar-a-v1"
  :dt 1d-3
  :duration 12
  :set ((rf-direction anterior))
  :record (:frames-every 0.01)
  :metrics-from 4
  :timeout 1800)

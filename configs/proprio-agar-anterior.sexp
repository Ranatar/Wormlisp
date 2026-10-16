;;;; Проприоцептивная цепь с полем вперёд и головным генератором (этап 12а) на агаре (этап 10): базовые параметры, 50 колец.
(config proprio-agar-anterior
  :model worm-3d-proprio
  :params "agar-a-v1"
  :dt 1d-3
  :duration 12
  :set ((rf-direction anterior) (avb-ventral 0.675) (head-bias 0.5))
  :record (:frames-every 0.01)
  :metrics-from 4
  :timeout 1800)

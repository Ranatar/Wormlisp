;;;; Уровень 1 в воде (этап 14): волна живого червя при плавании — частота и
;;;; длина волны Fang-Yen et al. 2010 (набор water-v1), 50 колец.
(config rhythm-water
  :model worm-3d
  :params "water-v1"
  :dt 5d-4
  :duration 8
  :record (:frames-every 0.01)
  :metrics-from 3
  :timeout 3600)

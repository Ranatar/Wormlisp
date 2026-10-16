;;;; Маленький прогон для тестов пакетного режима: 8 колец, агар.
(config tiny
  :model worm-3d-proprio
  :params "agar-a-v1"
  :rings 8
  :dt 1d-3
  :duration 0.3
  :record (:frames-every 0.01)
  :metrics-from 0
  :timeout 120)

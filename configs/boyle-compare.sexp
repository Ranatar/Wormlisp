;;;; Второй уровень для сверки с исходной моделью Бойла (план, этап 15):
;;;; бинарная цепь, вода как основа; среду задаёт ось medium развёрток,
;;;; линейно ведя C_t и C_n от воды к агару, как MEDIUM в WormSim.
(config boyle-compare
  :model worm-3d-proprio
  :params "water-v1"
  :dt 5d-4
  :duration 10
  :record (:frames-every 0.04)
  :metrics-from 4
  :timeout 3600)

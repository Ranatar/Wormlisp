;;;; Третий уровень с полем назад и заданным ритмом головы в воде (этап 12а).
(sweep head-drive-posterior
  :base #p"configs/connectome-water.sexp"
  :vary ((head-drive :list (3000 30000))
         (nmj-head-scale :list (0.3))
         (head-drive-frequency :list (1.7)))
  :duration 6
  :timeout 7200)

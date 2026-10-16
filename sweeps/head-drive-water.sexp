;;;; Проверка третьего уровня (этап 12а): поле вперёд и заданный ритм головы
;;;; в воде — повторяет ли тело изгиб головы бегущей волной.
(sweep head-drive-water
  :base #p"configs/connectome-water-anterior.sexp"
  :vary ((head-drive :list (1000 10000 100000))
         (nmj-head-scale :list (0.3 1))
         (head-drive-frequency :list (1.7)))
  :duration 6
  :timeout 7200)

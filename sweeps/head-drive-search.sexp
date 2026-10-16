;;;; Третий уровень с полем вперёд и заданным ритмом головы в воде (этап
;;;; 12а): при каком усилении тело повторяет голову бегущей волной.
(sweep head-drive-search
  :base #p"configs/connectome-water-anterior.sexp"
  :vary ((neuron-sr-conductance :geometric 1000 40000 :steps 2)
         (neuron-nmj-gain :geometric 3 12 :steps 2)
         (gap-motor-scale :geometric 0.02 0.5 :steps 2)
         (rf-length :linear 0.1 0.35 :steps 2)
         (sr-filter :geometric 50 250 :steps 2)
         (head-drive :list (10000))
         (nmj-head-scale :list (0.3))
         (head-drive-frequency :list (1.7)))
  :design (:latin-hypercube :samples 16 :seed 41)
  :duration 6
  :timeout 7200)

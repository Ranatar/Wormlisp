;;;; src/anatomy/package.lisp
;;;;
;;;; Слой 1. Анатомия: 95 мышц стенки тела — чтение, проверка, правило
;;;; раскладки вдоль тела и привязка к продольным рёбрам своего квадранта.
;;;; Нейроны и синапсы третьего уровня — connectome.lisp (этап 12).
;;;;
;;;; Пакет не зависит от worm.topology (тот же слой), поэтому привязка
;;;; получает положения колец и номера рёбер квадранта от вызывающего.

(defpackage #:worm.anatomy
  (:use #:cl #:worm.core)
  (:export
   ;; мышцы
   #:muscle #:make-muscle #:muscle-p #:muscle-name #:muscle-quadrant #:muscle-row
   #:muscle-index #:muscle-from #:muscle-peak #:muscle-to
   #:+muscle-quadrants+ #:+muscles-per-quadrant+ #:+cells-anterior-to-vulva+
   #:muscle-layout #:muscle-layout-p #:muscle-layout-head-start
   #:muscle-layout-vulva #:muscle-layout-tail-end #:muscle-layout-source
   #:generate-muscles #:muscle-profile
   ;; анатомия
   #:anatomy #:anatomy-p #:anatomy-muscles #:anatomy-layout #:anatomy-path
   #:anatomy-connectome
   ;; коннектом
   #:neuron #:make-neuron #:neuron-p #:neuron-name #:neuron-class #:neuron-transmitter
   #:neuron-position #:neuron-nmj
   #:connectome #:make-connectome #:connectome-p #:connectome-neurons #:connectome-chemical
   #:connectome-gap #:connectome-nmj #:connectome-source #:neuron-index
   #:read-connectome #:write-connectome-files
   #:load-anatomy #:read-muscles-file #:write-muscles-file #:validate-muscles
   #:default-muscles-path #:find-muscle
   ;; привязка
   #:muscle-binding #:muscle-binding-p #:muscle-binding-offsets
   #:muscle-binding-edges #:muscle-binding-weights #:muscle-binding-muscles
   #:bind-muscles #:muscle-edges
   ;; условия
   #:anatomy-invalid #:reload-anatomy))

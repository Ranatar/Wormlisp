;;;; src/topology/package.lisp
;;;;
;;;; Слой 1. Трубка N × K: нумерация узлов, массивы индексов семейств
;;;; элементов и величины покоя, посчитанные из профиля радиуса R(s).
;;;; Архитектура — «Представление модели»; дизайн — «Модель тела».

(defpackage #:worm.topology
  (:use #:cl #:worm.core)
  (:export
   ;; профиль
   #:ellipse-semi-axis #:ellipse-radius
   ;; трубка
   #:tube #:make-tube #:tube-p #:tube-rings #:tube-ring-nodes #:tube-length
   #:tube-radius-max #:tube-radius-end-min #:tube-semi-axis
   #:tube-ring-s #:tube-ring-fractions #:tube-ring-radius #:tube-ring-ds
   #:tube-rest-coords #:tube-node-count
   ;; семейства
   #:+families+ #:family-size #:family-ends #:family-rest
   #:tube-compartment-ring #:tube-rest-volume
   ;; узлы и квадранты
   #:node-index #:node-angle #:node-quadrant #:node-row #:+quadrants+
   #:quadrant-nodes #:long-edge-index #:quadrant-long-edges
   ;; геометрия
   #:compartment-volume #:frustum-volume #:ring-center #:ring-centers #:ring-curvatures))

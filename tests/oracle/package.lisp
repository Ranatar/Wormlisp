;;;; tests/oracle/package.lisp
;;;;
;;;; Ручные ядра тела и среды (план, этап 3). Модель пишется прямо в
;;;; интерфейсе compiled-model, без DSL: так физика работает раньше
;;;; кодогенерации, а на этапе 7 эти ядра становятся эталоном, с которым
;;;; сверяется сгенерированный код.
;;;;
;;;; Система worm/oracle вне слоёв архитектуры: это тестовая опора, как
;;;; worm/tests, и зависит она только от нижних слоёв и решателя.

(defpackage #:worm.oracle
  (:use #:cl #:worm.core)
  (:export
   ;; ядра
   #:edge-forces #:edge-matrix #:compartment-geometry #:pressure-forces #:pressure-matrix
   #:contact-forces #:contact-matrix #:drag-matrix #:ring-centers #:ring-tangents
   ;; модель
   #:body #:body-p #:build-body #:body-model #:body-tube #:body-anatomy #:body-binding
   #:body-params #:body-y0 #:body-pvec #:body-medium #:body-kernels
   #:body-long-stiffness #:body-rest-volume
   #:make-body-integrator #:set-body-param
   ;; размещение и наблюдаемые
   #:arc-coords #:place-on-surface #:ring-curvatures #:weighted-centroid
   #:volume-ratios #:+p-layout+))

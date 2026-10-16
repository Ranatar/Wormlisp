;;;; src/analysis/package.lisp
;;;;
;;;; Слой 6. Метрики походки по файлам прогона: средняя линия, знаковая
;;;; дорсо-вентральная кривизна, скорость центра тела, частота и длина
;;;; волны. Работает с файлами и не требует загруженной модели (архитектура,
;;;; «Модули и зависимости», правила).

(defpackage #:worm.analysis
  (:use #:cl #:worm.core)
  (:export
   #:run-data #:load-run #:run-data-meta #:run-data-frames #:run-data-rings #:run-data-ring-nodes
   #:run-data-length #:run-data-times
   #:curvature-series #:centroid-series #:heading-series
   #:gait-metrics #:gait-frequency #:gait-wavelength #:gait-speed #:gait-amplitude
   #:metrics-windows #:write-metrics-csv
   #:run-gait #:write-run-metrics #:write-sweep-summary
   #:wcon-object #:write-wcon))

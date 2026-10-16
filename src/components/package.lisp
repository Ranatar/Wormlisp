;;;; src/components/package.lisp
;;;;
;;;; Слой 3. Тело, среда, мышцы и управление первого уровня на языке модели
;;;; (этап 7); уровни 2 и 3 — этапы 10 и 12. Компонент — это только
;;;; уравнения: о решателе, потоках и файлах он не знает (архитектура,
;;;; «Модули и зависимости», правила).

(defpackage #:worm.components
  (:use #:cl #:worm.core)
  (:import-from #:worm.dsl #:define-component #:define-element-array #:defmodel)
  (:export
   #:worm-3d #:worm-3d-kinematic #:worm-3d-proprio
   #:edge-stiffness #:muscle-f-max #:muscle-lambda-min))

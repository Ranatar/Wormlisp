;;;; src/dsl/package.lisp
;;;;
;;;; Слой 2. Язык модели (архитектура, «DSL компонентов и defmodel»):
;;;; define-component описывает уравнения одного элемента семейства,
;;;; defmodel расставляет компоненты по семействам и связывает выходы со
;;;; входами. Раскрытие проверяет модель и даёт объект model-spec с
;;;; каноническим текстом и хэшем; понижение превращает компонент в ядро IR
;;;; (кодогенерация, шаги 2–4), а интерпретатор исполняет модель целиком —
;;;; эталон для кодогенерации этапа 7.

(defpackage #:worm.dsl
  (:use #:cl #:worm.core)
  (:import-from #:worm.ir #:ir+ #:ir- #:ir* #:ir/)
  (:export
   ;; компонент
   #:define-component #:parse-component #:find-component #:component
   #:component-name #:component-family #:component-params #:component-inputs
   #:component-outputs #:component-state #:component-source
   #:define-element-array #:find-element-array
   ;; семейства и отношения
   #:+families+ #:family-accepts-p #:find-relation
   ;; модель
   #:defmodel #:parse-model #:find-model #:model-spec #:model-spec-name
   #:model-spec-instances #:model-spec-wiring #:model-spec-subsystems
   #:model-spec-canonical-text #:model-spec-hash #:model-spec-components
   #:instance #:instance-name #:instance-component #:instance-family
   ;; понижение
   #:lower-component #:kernel #:kernel-graph #:kernel-outputs #:kernel-forces
   #:kernel-derivs #:kernel-damping #:kernel-leaves #:kernel-component
   ;; интерпретатор
   #:instantiate #:model-instance #:interpret #:interpretation
   #:interpretation-forces #:interpretation-outputs #:interpretation-derivs
   #:interpret-damping #:model-instance-topology #:model-instance-n-mech
   ;; условия
   #:dsl-location #:dsl-location-component #:dsl-location-form
   #:dsl-syntax-error #:unknown-name #:unknown-component #:unbound-input
   #:input-bound-twice #:algebraic-loop #:algebraic-loop-cycle #:family-mismatch
   #:model-unknown-parameter #:model-anatomy-invalid
   #:component-ir-type-error #:component-no-derivative))

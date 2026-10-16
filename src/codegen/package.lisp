;;;; src/codegen/package.lisp
;;;;
;;;; Слой 4. Кодогенерация (архитектура, «Кодогенерация»): model-spec →
;;;; compiled-model. Модель собирается тем же INSTANTIATE, что и для
;;;; эталонного интерпретатора, — топология, отношения связей, ядра IR и
;;;; поэлементные массивы общие; кодогенератор добавляет раскладку векторов,
;;;; карты рассеяния якобиана в ленту, ширину ленты по фактическим связям и
;;;; эмиссию типизированного Лиспа. Каждая функция модели порождается
;;;; фабрикой: код не зависит от числа колец, массивы передаются при сборке,
;;;; поэтому текст можно кэшировать в fasl.

(defpackage #:worm.codegen
  (:use #:cl #:worm.core)
  (:export
   #:compile-model #:+codegen-version+
   #:model-layout #:model-layout-param-names #:model-param-vector
   #:model-layout-state-offsets #:model-layout-neural-states
   #:model-instance-of #:model-activations #:model-param-names #:model-initial-state #:model-medium #:model-neural-states #:model-notes #:model-forms
   #:show-kernel #:interpret-kernel #:generated-form
   #:*cache-directory*))

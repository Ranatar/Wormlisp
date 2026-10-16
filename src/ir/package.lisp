;;;; src/ir/package.lisp
;;;;
;;;; Слой 1. Промежуточное представление выражений (архитектура,
;;;; «Кодогенерация», шаги 2–4): ациклический граф скалярных узлов.
;;;; Одинаковые узлы склеиваются при создании (hash-consing), поэтому общие
;;;; подвыражения выделяются сами; свёртка констант и тождества — тоже при
;;;; создании. Символьная производная, интерпретатор и примитивы с
;;;; написанной вручную производной (define-primitive).

(defpackage #:worm.ir
  (:use #:cl #:worm.core)
  (:export
   ;; граф и узлы
   #:graph #:make-graph #:*graph* #:with-graph #:graph-node-count
   #:node #:node-p #:node-id #:node-op #:node-args #:node-data
   #:const #:var #:const-p #:var-p #:const-value #:var-key #:zero-p #:one-p
   ;; арифметика
   #:ir+ #:ir- #:ir* #:ir/ #:ir-expt #:ir-sqrt #:ir-exp #:ir-log #:ir-tanh #:ir-step #:ir-sin #:ir-cos
   #:ir-square
   ;; примитивы
   #:define-primitive #:primitive #:find-primitive #:prim #:primitive-name
   #:primitive-arity #:primitive-value #:primitive-derivative
   #:positive #:clamp #:hill #:hill-dx #:logistic
   ;; производная и вычисление
   #:derivative #:gradient #:depends-on-p #:node-vars
   #:evaluate #:compile-evaluator #:evaluator #:evaluator-roots #:run-evaluator
   #:evaluator-variables
   #:node-sexp #:let-form
   ;; условия
   #:ir-type-error #:no-derivative #:no-derivative-primitive))

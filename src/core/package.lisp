;;;; src/core/package.lisp
;;;;
;;;; Слой 0. Общие вещи всего проекта: сведения о репозитории, типы
;;;; массивов горячего пути, корневые условия, журнал и структура
;;;; compiled-model — единственное, что решатель знает о модели.

(defpackage #:worm.core
  (:use #:cl)
  (:export
   ;; проект
   #:project-root #:git-revision
   ;; типы массивов
   #:dvec #:fvec #:ivec #:index
   #:make-dvec #:make-fvec #:make-ivec #:dvec-from #:ivec-from
   ;; числа
   #:+pi+ #:sq #:approx=
   ;; условия
   #:worm-error #:worm-error-message
   #:model-error #:codegen-error #:params-error
   #:numerical-error #:io-error #:protocol-error
   #:signal-worm-error
   ;; журнал
   #:*log-echo* #:*log-level* #:log-event #:log-path #:close-log
   ;; модель
   #:compiled-model #:make-compiled-model #:compiled-model-p
   #:compiled-model-name #:compiled-model-hash #:compiled-model-layout
   #:compiled-model-n-mech #:compiled-model-n-neural #:compiled-model-n-discrete
   #:compiled-model-n-observables #:compiled-model-n-syn #:compiled-model-n-aux
   #:compiled-model-lower-band #:compiled-model-upper-band
   #:compiled-model-topology #:compiled-model-anatomy
   #:compiled-model-observables #:compiled-model-neural-discrete
   #:compiled-model-neural-rhs #:compiled-model-neural-jacobian
   #:compiled-model-neural-output #:compiled-model-mech-forces
   #:compiled-model-mech-matrix #:compiled-model-check-state
   #:compiled-model-event-functions
   #:+clock-time+ #:+clock-h+ #:+clock-step+
   ;; лента
   #:band-ldab #:band-index #:make-band #:band-ref
   ;; плотная матрица
   #:dmat #:make-dmat))

;;;; src/solver/package.lisp
;;;;
;;;; Слой 4. Решатель: ленточная LU с частичным выбором ведущего элемента
;;;; (схема LAPACK dgbtrf) и плотная LU, линейно-неявный шаг Эйлера,
;;;; расщепление Ли на нервную и механическую подсистемы, кольцо
;;;; контрольных точек, условия и перезапуски с политикой как функцией от
;;;; условия. Решатель знает о модели только compiled-model из worm.core.
;;;; Архитектура — «Решатель», «Ошибки», «Производительность и память».

(defpackage #:worm.solver
  (:use #:cl #:worm.core)
  (:export
   ;; линейная алгебра
   #:band-factor! #:band-solve! #:band-multiply
   #:dense-factor! #:dense-solve!
   ;; условия и перезапуски
   #:numerical-blowup #:numerical-blowup-cause #:numerical-blowup-step
   #:numerical-blowup-time #:numerical-blowup-h #:numerical-blowup-where
   #:event-storm
   #:halve-timestep #:rollback-to-checkpoint #:clamp-and-continue #:abort-run
   ;; политики
   #:make-batch-policy #:never-policy
   ;; интегратор
   #:integrator #:make-integrator #:integrator-p
   #:integrator-model #:integrator-params #:integrator-y-mech #:integrator-y-neural
   #:integrator-z #:integrator-aux #:integrator-syn #:integrator-obs
   #:integrator-time #:integrator-step #:integrator-h
   #:integrator-dt-history #:integrator-restarts #:integrator-unreliable-p
   #:integrator-status #:integrator-neural-every
   #:step! #:run-steps #:invalidate-factorization #:set-integrator-position
   ;; контрольные точки
   #:save-checkpoint #:restore-checkpoint #:checkpoint-count #:checkpoint-steps
   #:latest-checkpoint-step))

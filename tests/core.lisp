;;;; tests/core.lisp — worm.platform и worm.core этапа 1.

(in-package #:worm.tests)

(def-suite core :in worm :description "Типы, условия, журнал, compiled-model, атомарная ячейка.")
(in-suite core)

(test array-constructors-have-declared-types
  (is (typep (worm.core:make-dvec 3) 'worm.core:dvec))
  (is (typep (worm.core:make-fvec 3) 'worm.core:fvec))
  (is (typep (worm.core:make-ivec 3) 'worm.core:ivec))
  (is (equalp #(1d0 2d0) (worm.core:dvec-from '(1 2))))
  (is (typep (worm.core:dvec-from '(1/2 3)) '(simple-array double-float (2)))))

(test condition-hierarchy-has-one-root
  (dolist (type '(worm.core:model-error worm.core:codegen-error worm.core:params-error
                  worm.core:numerical-error worm.core:io-error worm.core:protocol-error
                  worm.anatomy:anatomy-invalid worm.params:unknown-parameter
                  worm.params:missing-value worm.params:unit-mismatch worm.params:out-of-bounds))
    (is (subtypep type 'worm.core:worm-error) "~S не наследует worm-error" type))
  (is (subtypep 'worm.params:unknown-parameter 'worm.core:model-error))
  (is (subtypep 'worm.anatomy:anatomy-invalid 'worm.core:model-error))
  (is (subtypep 'worm.params:missing-value 'worm.core:params-error)))

(test worm-error-reports-its-message
  (handler-case (worm.core:signal-worm-error 'worm.core:io-error "диск ~D" 7)
    (worm.core:io-error (e) (is (string= "диск 7" (princ-to-string e))))))

(test log-event-writes-a-line
  (let ((marker (format nil "проверка журнала ~D" (random 1000000))))
    (worm.core:log-event :info 'tests "~A" marker)
    (worm.core:close-log)
    (is (search marker (uiop:read-file-string (worm.core:log-path))))))

(test log-respects-level
  (let ((marker (format nil "скрытое событие ~D" (random 1000000)))
        (worm.core:*log-level* :warn))
    (worm.core:log-event :info 'tests "~A" marker)
    (worm.core:close-log)
    (is (not (search marker (uiop:read-file-string (worm.core:log-path)))))))

(test compiled-model-holds-sizes-and-functions
  (let ((m (worm.core:make-compiled-model :name 'toy :n-mech 6 :lower-band 3 :upper-band 3
                                          :mech-forces (lambda (y p in g) (declare (ignore y p in)) g))))
    (is (= 6 (worm.core:compiled-model-n-mech m)))
    (is (functionp (worm.core:compiled-model-mech-forces m)))
    (is (null (worm.core:compiled-model-neural-rhs m)))
    (is (search "TOY" (princ-to-string m)))))

(test atomic-cell-swaps-and-compares
  (let ((cell (worm.platform:make-atomic-cell :a)))
    (is (eq :a (worm.platform:atomic-cell-swap cell :b)))
    (is (eq :b (worm.platform:atomic-cell-value cell)))
    (is (eq :b (worm.platform:atomic-cell-compare-and-swap cell :x :c)))
    (is (eq :b (worm.platform:atomic-cell-value cell)) "CAS с чужим старым значением не пишет")
    (worm.platform:atomic-cell-compare-and-swap cell :b :c)
    (is (eq :c (worm.platform:atomic-cell-value cell)))))

(test float-traps-are-on-by-default
  (let ((traps (worm.platform:enabled-float-traps)))
    (is (subsetp '(:overflow :invalid :divide-by-zero) traps) "ловушки: ~S" traps))
  (is (sb-ext:float-infinity-p
       (worm.platform:with-float-traps-masked (:divide-by-zero)
         (/ 1d0 (- 1d0 1d0))))))

(test md5-matches-known-values
  (is (string= "d41d8cd98f00b204e9800998ecf8427e" (worm.platform:md5-hex "")))
  (is (string= "900150983cd24fb0d6963f7d28e17f72" (worm.platform:md5-hex "abc"))))

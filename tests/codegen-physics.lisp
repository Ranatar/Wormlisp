;;;; tests/codegen-physics.lisp — прогоны этапов 3 и 5 на сгенерированном коде
;;;; (этап 7, make test-full).
;;;;
;;;; Модели worm-3d и worm-3d-kinematic на языке модели идут шаг в шаг с
;;;; ручной моделью тех же наборов параметров на 50 колец; траектории
;;;; совпадают до округления, поэтому совпадают и метрики походки этапов 3
;;;; и 5, проверенные в tests/physics.lisp.
;;;;
;;;; Вода — с h = 1·10⁻⁴. При h ≥ 2,5·10⁻⁴ вода (и с мышцами, и с
;;;; натяжением) усиливает ошибку округления: две ручные модели, разошедшиеся
;;;; на 10⁻¹² мм, к t ≈ 0,5 с расходятся на 10⁻⁴–10⁻² мм, а сгенерированная
;;;; модель при 5·10⁻⁴ в одном из таких расхождений сминает отсек. При
;;;; 1·10⁻⁴ расхождение остаётся 10⁻¹¹ (план, этап 7, находки).

(in-package #:worm.tests)

(in-suite physics)

(defun lockstep-divergence (params &key muscles h time)
  "Наибольшее расхождение y-mech (мм) и y-neural сгенерированной и ручной
моделей за TIME секунд с шагом H."
  (let* ((gen (worm.codegen:compile-model (if muscles 'worm.components:worm-3d 'worm.components:worm-3d-kinematic)
                                          :params params))
         (a (worm.oracle:make-body-integrator (oracle-like params :rings 50 :muscles muscles) :h h))
         (b (worm.runtime:make-model-integrator gen :h h))
         (worst 0d0) (worst-neural 0d0))
    (loop repeat (round time (* 100 h))
          do (worm.solver:run-steps a 100)
             (worm.solver:run-steps b 100)
             (setf worst (max worst (max-diff (worm.solver:integrator-y-mech a) (worm.solver:integrator-y-mech b)))
                   worst-neural (max worst-neural (max-diff (worm.solver:integrator-y-neural a)
                                                            (worm.solver:integrator-y-neural b)))))
    (is (= (worm.solver:integrator-step a) (worm.solver:integrator-step b)))
    (values worst worst-neural)))

(test generated-runs-repeat-the-oracle-on-agar
  (dolist (muscles '(nil t))
    (multiple-value-bind (y yn) (lockstep-divergence "agar-a-v1" :muscles muscles :h 1d-3 :time 4d0)
      (is (< y 1d-10) "агар, ~:[натяжение~;мышцы~]: траектории разошлись на ~,2E мм" muscles y)
      (is (< yn 1d-10) "агар: нервно-мышечная часть разошлась на ~,2E" yn))))

(test generated-runs-repeat-the-oracle-in-water
  (dolist (muscles '(nil t))
    (multiple-value-bind (y yn) (lockstep-divergence "water-v1" :muscles muscles :h 1d-4 :time 0.6d0)
      (is (< y 1d-9) "вода, ~:[натяжение~;мышцы~]: траектории разошлись на ~,2E мм" muscles y)
      (is (< yn 1d-9) "вода: нервно-мышечная часть разошлась на ~,2E" yn))))

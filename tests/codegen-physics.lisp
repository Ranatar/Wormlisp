;;;; tests/codegen-physics.lisp — прогоны этапов 3 и 5 на сгенерированном коде
;;;; (этап 7, make test-full).
;;;;
;;;; Модели worm-3d и worm-3d-kinematic на языке модели идут шаг в шаг с
;;;; ручной моделью тех же наборов параметров на 50 колец; траектории
;;;; совпадают до округления, поэтому совпадают и метрики походки этапов 3
;;;; и 5, проверенные в tests/physics.lisp.
;;;;
;;;; Сверка идёт без производной вязких сил по положению в матрице шага
;;;; (решение 26): у ручных ядер её нет. Вода при этом — с h = 1·10⁻⁴: без
;;;; поправки при h ≥ 2,5·10⁻⁴ ошибка округления растёт до 10⁻⁴–10⁻² мм
;;;; (этап 7, находки; причина найдена на этапе 10). Последний тест — та же
;;;; вода с поправкой при 5·10⁻⁴.

(in-package #:worm.tests)

(in-suite physics)

(defun lockstep-divergence (params &key muscles h time)
  "Наибольшее расхождение y-mech (мм) и y-neural сгенерированной и ручной
моделей за TIME секунд с шагом H."
  (let* ((gen (worm.codegen:compile-model (if muscles 'worm.components:worm-3d 'worm.components:worm-3d-kinematic)
                                          :params params :damping-geometry nil))
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

(test water-is-stable-with-damping-geometry
  ;; Решение 26: без производной вязких сил по положению вода при
  ;; h ≥ 2,5·10⁻⁴ усиливала ошибку округления до 10⁻⁴ мм; с ней две копии
  ;; прогона, разошедшиеся на 10⁻¹² мм, остаются рядом и при h = 5·10⁻⁴.
  (dolist (model '(worm.components:worm-3d-kinematic worm.components:worm-3d))
    (let* ((gen (worm.codegen:compile-model model :params "water-v1"))
           (a (worm.runtime:make-model-integrator gen :h 5d-4))
           (b (worm.runtime:make-model-integrator gen :h 5d-4))
           (worst 0d0))
      (incf (aref (worm.solver:integrator-y-mech b) 601) 1d-12)
      (loop repeat 24
            do (worm.solver:run-steps a 100) (worm.solver:run-steps b 100)
               (setf worst (max worst (max-diff (worm.solver:integrator-y-mech a) (worm.solver:integrator-y-mech b)))))
      (is (< worst 1d-9) "~(~A~): расхождение ~,2E мм за 1,2 с" model worst))))

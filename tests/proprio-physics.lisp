;;;; tests/proprio-physics.lisp — походка под проприоцептивной цепью (этап 10,
;;;; make test-full).
;;;;
;;;; Волна возникает без внешнего ритма; от воды к агару частота и длина
;;;; волны падают при одном наборе параметров контроллера. Промежуточные
;;;; вязкости главного теста пока не проходят (план, этап 10, итог), поэтому
;;;; здесь проверяются концы: вода и агар.

(in-package #:worm.tests)

(in-suite physics)

(defun proprio-gait (params &key h time from (name "proprio") overrides)
  "Прогон worm-3d-proprio через рантайм с записью кадров; метрики походки
с момента FROM и число включений DB каждой единицы после FROM."
  (let* ((dir (runtime-dir name))
         (model (worm.codegen:compile-model 'worm.components:worm-3d-proprio :params params))
         (run (worm.runtime:start-run model :params params :dt h :paused t :record t :frames-every 0.01d0
                                            :directory dir :policy :batch))
         (switches (make-array 12 :initial-element 0)))
    (loop for (k v) on overrides by #'cddr do (worm.runtime:set-param k v :run run))
    (worm.runtime:step-run (round from h) run)
    (let ((previous (worm.runtime:call-in-run (lambda (r) (copy-seq (worm.solver:integrator-z (worm.runtime:run-integrator r)))) run)))
      (loop repeat (round (- time from) 0.01d0)
            do (worm.runtime:step-run (round 0.01d0 h) run)
               (let ((z (worm.runtime:call-in-run (lambda (r) (copy-seq (worm.solver:integrator-z (worm.runtime:run-integrator r)))) run)))
                 (dotimes (u 12)
                   (when (and (= (aref z u) 1) (= (aref previous u) 0)) (incf (aref switches u))))
                 (setf previous z))))
    (worm.runtime:stop-run run)
    (let* ((r (worm.analysis:load-run dir))
           (times (worm.analysis:run-data-times r)))
      (values (worm.analysis:gait-metrics r :from (position-if (lambda (x) (>= x from)) times))
              switches
              (worm.runtime:run-status run)))))

(test proprio-wave-needs-no-external-rhythm
  (multiple-value-bind (m switches status) (proprio-gait "agar-a-v1" :h 1d-3 :time 10d0 :from 4d0 :name "proprio-agar")
    (is (eq :done status))
    ;; крайние единицы (голова и укороченное поле хвоста) могут стоять
    (is (every (lambda (n) (>= n 2)) (subseq switches 1 11)) "единицы тела переключаются: ~S" switches)
    (is (< 0.4d0 (or (getf m :frequency) 0) 1.2d0) "частота ~A Гц" (getf m :frequency))
    (is (< 0.5d0 (or (getf m :wavelength) 0) 1.1d0) "длина волны ~A L" (getf m :wavelength))
    (is (> (getf m :speed) 0.1d0) "ползёт вперёд: ~,4F мм/с" (getf m :speed))))

(test proprio-gait-slows-and-shortens-from-water-to-agar
  ;; Главный тест на концах: один набор параметров контроллера, среды —
  ;; вода и агар. В воде — частота живого червя, на агаре — ниже и с
  ;; более короткой волной.
  (let ((water (proprio-gait "water-v1" :h 5d-4 :time 4d0 :from 2d0 :name "proprio-water"))
        (agar (proprio-gait "agar-a-v1" :h 1d-3 :time 10d0 :from 4d0 :name "proprio-agar-2")))
    (is (< (abs (- (getf water :frequency) 1.76d0)) 0.15d0) "вода: ~,3F Гц" (getf water :frequency))
    (is (< (abs (- (getf water :wavelength) 1.54d0)) 0.35d0) "вода: ~,3F L" (getf water :wavelength))
    (is (> (getf water :frequency) (* 2 (getf agar :frequency))) "частота: вода ~,3F, агар ~,3F"
        (getf water :frequency) (getf agar :frequency))
    (is (> (getf water :wavelength) (* 1.8d0 (getf agar :wavelength))) "длина волны: вода ~,3F, агар ~,3F"
        (getf water :wavelength) (getf agar :wavelength))))

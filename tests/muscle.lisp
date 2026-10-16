;;;; tests/muscle.lisp — усреднённая мышца и генератор ритма (этап 5).
;;;;
;;;; Условия готовности: ступенька входа при фиксированной длине — кальций
;;;; растёт с постоянной τ_Ca, сила выходит на плато; развёртки по длине и
;;;; скорости воспроизводят f_L и f_V; откат к контрольной точке и
;;;; продолжение побитово совпадают с прогоном без остановки.
;;;;
;;;; Ступенька и развёртки идут через настоящие функции модели — наблюдаемые
;;;; длины и скорости, кальций и сглаженная скорость в нервно-мышечной части,
;;;; натяжение на выходе, — но при замороженной механике: стенд подменяет
;;;; механику одной фиктивной переменной, а геометрию тела и скорости узлов
;;;; задаёт сам.

(in-package #:worm.tests)

(def-suite muscle :in worm :description "Мышца и генератор ритма.")
(in-suite muscle)

;;; Формы f_L и f_V

(test force-length-is-a-bell
  (let ((lmin 0.35d0))
    (is (= 1d0 (worm.oracle:force-length 1d0 lmin)))
    (is (= 0d0 (worm.oracle:force-length lmin lmin)) "ноль при предельном укорочении")
    (is (= 0d0 (worm.oracle:force-length 0.2d0 lmin)))
    (is (worm.core:approx= (worm.oracle:force-length 0.8d0 lmin) (worm.oracle:force-length 1.2d0 lmin)
                           :rel 1d-14))
    (is (< (worm.oracle:force-length 0.6d0 lmin) (worm.oracle:force-length 0.8d0 lmin) 1d0))))

(test force-velocity-is-a-hill-hyperbola
  (let ((v-max 30d0) (a 0.25d0) (e 1.5d0))
    (is (= 1d0 (worm.oracle:force-velocity 0d0 v-max a e)))
    (is (= 0d0 (worm.oracle:force-velocity (- v-max) v-max a e)) "ноль при v_max")
    (is (= 0d0 (worm.oracle:force-velocity (* -2 v-max) v-max a e)))
    ;; уравнение Хилла в нормированном виде: (f + a)(u + a) = a(1 + a)
    (loop for u in '(0.05d0 0.2d0 0.5d0 0.9d0)
          for f = (worm.oracle:force-velocity (- (* u v-max)) v-max a e)
          do (is (worm.core:approx= (* (+ f a) (+ u a)) (* a (+ 1 a)) :rel 1d-12) "Хилл при u = ~A" u))
    ;; при удлинении растёт и насыщается до E, наклон в нуле непрерывен
    (is (< 1d0 (worm.oracle:force-velocity 3d0 v-max a e) (worm.oracle:force-velocity 30d0 v-max a e) e))
    (is (< (- e (worm.oracle:force-velocity 1d6 v-max a e)) 1d-4))
    (let* ((d 1d-6)
           (left (/ (- 1d0 (worm.oracle:force-velocity (- d) v-max a e)) d))
           (right (/ (- (worm.oracle:force-velocity d v-max a e) 1d0) d)))
      (is (worm.core:approx= left right :rel 1d-4) "наклон слева ~,4F, справа ~,4F" left right)
      (is (worm.core:approx= left (/ (+ 1 a) (* a v-max)) :rel 1d-4)))))

(test calcium-and-activation-curves
  (is (= 0.5d0 (worm.oracle:ca-steady 0.5d0 0.5d0 0.15d0)) "Ca∞(I½) = 1/2")
  (is (< (worm.oracle:ca-steady 0d0 0.5d0 0.15d0) 0.05d0) "без входа кальций почти нулевой")
  (is (> (worm.oracle:ca-steady 1d0 0.5d0 0.15d0) 0.95d0) "полный вход — почти насыщение")
  (is (= 0.5d0 (worm.oracle:hill-activation 0.5d0 0.5d0 3d0)) "a(K_Ca) = 1/2")
  (is (= 0d0 (worm.oracle:hill-activation -1d-9 0.5d0 3d0)) "отрицательный кальций — ноль"))

(test f-max-follows-boyle-gradient
  (let* ((body (worm.oracle:build-body :params "water-v1" :muscles t))
         (f (worm.oracle:body-f-max body))
         (muscles (worm.anatomy:muscle-binding-muscles (worm.oracle:body-binding body))))
    (flet ((cell (name) (aref f (position name muscles :key #'worm.anatomy:muscle-name))))
      (is (> (cell :mdl03) (cell :mdl13) (cell :mdl23)) "убывает от головы к хвосту")
      (is (worm.core:approx= (/ (cell :mdl01) (cell :mdl03)) (* 2/3 (/ (+ 0.69d0 (* -0.4d0 (worm.anatomy:muscle-peak (aref muscles 0))))
                                                                         (+ 0.69d0 (* -0.4d0 (worm.anatomy:muscle-peak (aref muscles (position :mdl03 muscles :key #'worm.anatomy:muscle-name)))))))
                             :rel 1d-12)
          "первая клетка ряда ослаблена на 2/3")
      (let ((mid (worm.params:param-value (worm.oracle:body-params body) :f-max-mid)))
        (is (< 0.9d0 (/ (cell :mvr12) mid) 1.2d0) "середина тела — около F_max середины")))))

;;; Стенд: нервно-мышечная часть тела при замороженной геометрии

(defun muscle-bench (body y aux)
  "Модель из нервно-мышечных функций BODY при геометрии Y и скоростях AUX."
  (let ((m (worm.oracle:body-model body)))
    (worm.core:make-compiled-model
     :name 'muscle-bench :n-mech 1 :n-neural (worm.core:compiled-model-n-neural m)
     :n-observables (worm.core:compiled-model-n-observables m) :n-syn (worm.core:compiled-model-n-syn m)
     :observables (lambda (yy a p obs)
                    (declare (ignore yy a))
                    (funcall (worm.core:compiled-model-observables m) y aux p obs))
     :neural-rhs (worm.core:compiled-model-neural-rhs m)
     :neural-jacobian (worm.core:compiled-model-neural-jacobian m)
     :neural-output (worm.core:compiled-model-neural-output m)
     :mech-forces (lambda (yy syn p g) (declare (ignore yy syn p g)) (values))
     :mech-matrix (lambda (yy syn p clock ab)
                    (declare (ignore yy syn p clock))
                    (incf (aref ab (worm.core:band-index 0 0 0 0)) 1d0)
                    (values)))))

(defun bench-run (&key (scale 1d0) (rate 0d0) (time 0.6d0) (h 1d-3))
  "Постоянный полный вход дорсальных клеток (волна бесконечной длины в фазе
максимума), тело растянуто в SCALE раз, все рёбра удлиняются со
скоростью RATE (1/с). Возвращает интегратор и тело."
  (let* ((body (worm.oracle:build-body :params "water-v1" :muscles t :frequency 0d0
                                       :wavelength 1d9 :phase (/ pi 2) :amplitude 1d0))
         (y0 (worm.oracle:body-y0 body))
         (y (map 'worm.core:dvec (lambda (x) (* scale x)) y0))
         (aux (map 'worm.core:dvec (lambda (x) (* rate x)) y))
         (it (worm.solver:make-integrator (muscle-bench body y aux)
                                          :params (worm.oracle:body-pvec body)
                                          :y-neural (worm.oracle:body-y0-neural body)
                                          :h h :neural-every 10 :checkpoint-every 100)))
    (worm.solver:run-steps it (round time h))
    (values it body)))

(defun dorsal-p (muscle)
  (member (worm.anatomy:muscle-quadrant muscle) '(:dorsal-left :dorsal-right)))

(test step-input-raises-calcium-with-tau-ca
  ;; Линейно-неявный Эйлер для τ·Ċa = Ca∞ − Ca даёт точно
  ;; Ca_n − Ca∞ = (Ca₀ − Ca∞)/(1 + h/τ)ⁿ, а это e^{−t/τ} с ошибкой O(h).
  (let* ((h 1d-3) (n 300) (tau 0.1d0))
    (multiple-value-bind (it body) (bench-run :time (* n h) :h h)
      (let* ((p (worm.oracle:body-pvec body))
             (ca (worm.solver:integrator-y-neural it))
             (muscles (worm.anatomy:muscle-binding-muscles (worm.oracle:body-binding body)))
             (ca0 (aref (worm.oracle:body-y0-neural body) 0))
             (ca-inf (worm.oracle:ca-steady 1d0 (aref p (position :syn-half worm.oracle:+p-layout+))
                                            (aref p (position :syn-width worm.oracle:+p-layout+))))
             (discrete (+ ca-inf (/ (- ca0 ca-inf) (expt (+ 1 (/ h tau)) n))))
             (continuous (+ ca-inf (* (- ca0 ca-inf) (exp (- (/ (* n h) tau)))))))
        (dotimes (m (length muscles))
          (if (dorsal-p (aref muscles m))
              (is (worm.core:approx= (aref ca m) discrete :rel 1d-12) "клетка ~D: ~,6F" m (aref ca m))
              (is (= ca0 (aref ca m)) "вентральная клетка без входа")))
        (is (< (abs (- discrete continuous)) (* 2 (/ h tau) (abs (- ca0 ca-inf)))) "первый порядок")))))

(test force-plateaus-at-f-max-times-activation
  (multiple-value-bind (it body) (bench-run :time 1.5d0)
    (let* ((p (worm.oracle:body-pvec body))
           (syn (worm.solver:integrator-syn it))
           (f-max (worm.oracle:body-f-max body))
           (muscles (worm.anatomy:muscle-binding-muscles (worm.oracle:body-binding body)))
           (ca-inf (worm.oracle:ca-steady 1d0 0.5d0 0.15d0))
           (a (worm.oracle:hill-activation ca-inf (aref p (position :ca-half worm.oracle:+p-layout+))
                                           (aref p (position :hill-n worm.oracle:+p-layout+)))))
      (dotimes (m (length muscles))
        (when (dorsal-p (aref muscles m))
          (is (worm.core:approx= (aref syn m) (* (aref f-max m) a) :rel 1d-5)
              "клетка ~D: ~,4F мкН против ~,4F" m (aref syn m) (* (aref f-max m) a)))))))

(test length-sweep-reproduces-f-l
  (let ((base (worm.solver:integrator-syn (bench-run :scale 1d0))))
    (dolist (scale '(0.8d0 0.9d0 1.1d0))
      (multiple-value-bind (it body) (bench-run :scale scale)
        (let ((syn (worm.solver:integrator-syn it))
              (lmin (worm.oracle:body-lambda-min body))
              (muscles (worm.anatomy:muscle-binding-muscles (worm.oracle:body-binding body))))
          (dotimes (m (length muscles))
            (when (dorsal-p (aref muscles m))
              (is (worm.core:approx= (/ (aref syn m) (aref base m))
                                     (worm.oracle:force-length scale (aref lmin m)) :rel 1d-10)
                  "λ = ~,2F, клетка ~D" scale m))))))))

(test velocity-sweep-reproduces-f-v
  ;; Сглаженная скорость за 0,6 с (60 τ_v) сходится к заданной.
  (let ((base (worm.solver:integrator-syn (bench-run))))
    (dolist (rate '(-6d0 -1.5d0 2d0 10d0))
      (multiple-value-bind (it body) (bench-run :rate rate)
        (let* ((p (worm.oracle:body-pvec body))
               (expected (worm.oracle:force-velocity rate (aref p (position :v-max worm.oracle:+p-layout+))
                                                     (aref p (position :hill-a-ratio worm.oracle:+p-layout+))
                                                     (aref p (position :hill-eccentric worm.oracle:+p-layout+))))
               (syn (worm.solver:integrator-syn it))
               (muscles (worm.anatomy:muscle-binding-muscles (worm.oracle:body-binding body))))
          (dotimes (m (length muscles))
            (when (dorsal-p (aref muscles m))
              (is (worm.core:approx= (/ (aref syn m) (aref base m)) expected :rel 1d-9)
                  "λ̇ = ~,1F 1/с, клетка ~D: ~,6F против ~,6F" rate m (/ (aref syn m) (aref base m)) expected))))))))

;;; Мышцы в теле

(defun muscle-observables (model y aux p)
  (let ((obs (worm.core:make-dvec (worm.core:compiled-model-n-observables model))))
    (funcall (worm.core:compiled-model-observables model) y aux p obs)
    obs))

(test muscle-rate-is-the-derivative-of-its-length
  ;; Скорость длины клетки по скоростям узлов прошлого шага (aux) совпадает
  ;; с разностью её длин за этот шаг с точностью O(h).
  (let* ((body (worm.oracle:build-body :params "water-v1" :muscles t :ramp 0.1d0))
         (model (worm.oracle:body-model body))
         (it (worm.oracle:make-body-integrator body :h 2d-4)))
    (worm.solver:run-steps it 1500)
    (let* ((p (worm.solver:integrator-params it))
           (y0 (copy-seq (worm.solver:integrator-y-mech it)))
           (before (muscle-observables model y0 (worm.solver:integrator-aux it) p)))
      (worm.solver:step! it)
      (let* ((after (muscle-observables model (worm.solver:integrator-y-mech it)
                                        (worm.solver:integrator-aux it) p))
             (worst 0d0) (scale 0d0))
        (dotimes (m 95)
          (let ((fd (/ (- (aref after (+ 50 m)) (aref before (+ 50 m))) 2d-4))
                (rate (aref after (+ 50 95 m))))
            (setf worst (max worst (abs (- fd rate))) scale (max scale (abs fd)))))
        (is (> scale 0.1d0) "клетки двигаются: |λ̇| до ~,2F 1/с" scale)
        (is (< worst (* 0.01d0 scale)) "расхождение ~,2E при размахе ~,2E" worst scale)))))

(defun body-state (it)
  (list (copy-seq (worm.solver:integrator-y-mech it)) (copy-seq (worm.solver:integrator-y-neural it))
        (copy-seq (worm.solver:integrator-aux it)) (worm.solver:integrator-time it)
        (worm.solver:integrator-step it)))

(defun muscle-integrator ()
  (let ((body (worm.oracle:build-body :params "water-v1" :muscles t :ramp 0.05d0 :rings 20)))
    (worm.oracle:make-body-integrator body :h 5d-4 :checkpoint-every 100 :neural-every 10)))

(test rollback-and-continue-is-bitwise-identical
  (let ((clean (muscle-integrator))
        (interrupted (muscle-integrator)))
    (worm.solver:run-steps clean 300)
    (worm.solver:run-steps interrupted 250)
    (worm.solver:restore-checkpoint interrupted)
    (is (= 200 (worm.solver:integrator-step interrupted)))
    (worm.solver:run-steps interrupted 100)
    (is (equalp (body-state clean) (body-state interrupted))
        "координаты, кальций, сглаженные скорости и aux совпадают побитово")))

(test disk-checkpoint-continues-bitwise
  (let ((clean (muscle-integrator))
        (first-half (muscle-integrator))
        (path (merge-pathnames "cache/test-io/muscle.ckpt" (worm.core:project-root))))
    (worm.solver:run-steps clean 300)
    (worm.solver:run-steps first-half 200)
    (worm.io:write-checkpoint-file path :model-hash "oracle-muscles" :layout-hash "rings-20"
                                        :step (worm.solver:integrator-step first-half)
                                        :time (worm.solver:integrator-time first-half)
                                        :h (worm.solver:integrator-h first-half)
                                        :vectors (list (cons "y-mech" (worm.solver:integrator-y-mech first-half))
                                                       (cons "y-neural" (worm.solver:integrator-y-neural first-half))
                                                       (cons "aux" (worm.solver:integrator-aux first-half))))
    (let* ((c (worm.io:read-checkpoint-file path :model-hash "oracle-muscles"))
           (v (getf c :vectors))
           (template (muscle-integrator))
           (resumed (worm.solver:make-integrator (worm.solver:integrator-model template)
                                                 :params (worm.solver:integrator-params template)
                                                 :y-mech (cdr (assoc "y-mech" v :test #'string=))
                                                 :y-neural (cdr (assoc "y-neural" v :test #'string=))
                                                 :aux (cdr (assoc "aux" v :test #'string=))
                                                 :time (getf c :time) :step (getf c :step) :h (getf c :h)
                                                 :checkpoint-every 100 :neural-every 10)))
      (worm.solver:run-steps resumed 100)
      (is (equalp (body-state clean) (body-state resumed)) "продолжение с диска побитово как без остановки"))))

(test muscle-steps-allocate-nothing
  ;; Как в tests/solver.lisp: учёт выделения блоками по 32 КБ, поэтому шагов
  ;; столько, чтобы одно упакованное число на шаг дало больше блока.
  (let ((it (muscle-integrator)))
    (worm.solver:run-steps it 200)
    (let ((bytes (bytes-consed-by (lambda () (loop repeat 3000 do (worm.solver:step! it))))))
      (is (<= bytes 32768) "3000 шагов тела с мышцами выделили ~D байт" bytes))))

;;;; tests/physics.lisp — тесты тела и среды из дизайна (make test-full).
;;;;
;;;; Условия готовности этапа 3 (дизайн, «Валидация», тесты звеньев):
;;;;   изогнутое тело без мышц расслабляется к прямому монотонно;
;;;;   при сильном изгибе объём каждого отсека меняется меньше чем на 1%;
;;;;   при K = 1 волна не даёт поступательного движения;
;;;;   обращение направления волны меняет знак движения;
;;;;   заданная волна натяжения даёт движение вперёд и в воде, и на агаре.
;;;; Это прогоны ручной модели на 50 колец, поэтому они в make test-full.

(in-package #:worm.tests)

(def-suite physics :description "Физика тела и среды на ручных ядрах (make test-full).")
(in-suite physics)

(defun integrator-y (it) (worm.solver:integrator-y-mech it))

(defun run-body (body &key (h 5d-4) (time 1d0) (every 50) on-sample)
  "Прогнать модель тела TIME секунд; ON-SAMPLE вызывается каждые EVERY шагов."
  (let ((it (worm.oracle:make-body-integrator body :h h))
        (steps (round time h)))
    (loop for done from 0 below steps by every
          do (worm.solver:run-steps it (min every (- steps done)))
             (when on-sample (funcall on-sample it)))
    it))

(defun centroid-shift (body it)
  (multiple-value-bind (x0 y0 z0) (worm.oracle:weighted-centroid (worm.oracle:body-y0 body)
                                                                 (worm.oracle:body-tube body))
    (multiple-value-bind (x1 y1 z1) (worm.oracle:weighted-centroid (integrator-y it)
                                                                   (worm.oracle:body-tube body))
      (values (- x1 x0) (- y1 y0) (- z1 z0)))))

(defun mid-curvature (it)
  (let ((curv (worm.core:make-dvec 50)))
    (worm.oracle:ring-curvatures (integrator-y it) 50 8 (worm.core:make-dvec 150) curv)
    (aref curv 25)))

(test bent-body-relaxes-to-straight-monotonically
  (let* ((body (worm.oracle:build-body :params "water-v1" :initial-curvature 4d0))
         (samples '()))
    (run-body body :h 1d-4 :time 0.06d0 :every 20
                   :on-sample (lambda (it) (push (mid-curvature it) samples)))
    (setf samples (nreverse samples))
    ;; через 2 мс при постоянной около τ_c = 5 мс остаётся около 4·e^(−0,4) ≈ 2,7
    (is (< 2d0 (first samples) 4d0) "расслабление постепенное, а не мгновенное: ~,3F" (first samples))
    (is (every #'> samples (rest samples)) "кривизна убывает монотонно")
    (is (every #'plusp samples) "без колебаний: знак не меняется")
    (is (< (car (last samples)) 0.01d0) "через 60 мс тело прямое: ~,4F" (car (last samples)))))

(test strong-bend-keeps-compartment-volumes
  ;; Чистый изгибающий момент: одна сторона тянет, другая толкает, продольной
  ;; силы нет. Кривизна в середине — около 7,8/мм, больше, чем у живого червя
  ;; (κ·L до 6,7, Fang-Yen et al. 2010); хуже всего — мягкие отсеки хвоста.
  ;; При 9,6/мм они меняют объём на 1,4% (этап 5, после калибровки EI и p₀).
  (let* ((body (worm.oracle:build-body :params "water-v1" :amplitude 16.5d0 :frequency 0d0
                                       :wavelength 1d9 :phase (/ pi 2) :antagonist t :ramp 0.05d0))
         (worst 0d0) (it nil))
    (setf it (run-body body :h 5d-4 :time 0.2d0 :every 20
                            :on-sample (lambda (it)
                                         (setf worst (max worst (reduce #'max (worm.oracle:volume-ratios (integrator-y it) (worm.oracle:body-tube body))
                                                                        :key (lambda (r) (abs (- r 1)))))))))
    (is (> (mid-curvature it) 7d0) "изгиб сильнее, чем у живого червя: κ = ~,2F 1/мм" (mid-curvature it))
    (is (< worst 0.01d0) "объём отсеков меняется на ~,2F%" (* 100 worst))))

(defun swim (&key (params "water-v1") (amplitude 20d0) (frequency 2d0) (wavelength 1.5d0)
                  (direction 1d0) (time 0.75d0) (h 5d-4) c-t c-n)
  (let ((body (worm.oracle:build-body :params params :amplitude amplitude :frequency frequency
                                      :wavelength wavelength :direction direction
                                      :ramp (/ 1d0 frequency))))
    (when c-t (worm.oracle:set-body-param (worm.oracle:body-pvec body) :c-t c-t))
    (when c-n (worm.oracle:set-body-param (worm.oracle:body-pvec body) :c-n c-n))
    (let ((it (run-body body :h h :time time :every 100)))
      (centroid-shift body it))))

(test isotropic-drag-gives-no-thrust
  ;; При K = 1 сумма внутренних сил равна нулю, и центр тела с весами Δs не
  ;; сдвигается вовсе — проверка до ошибок округления.
  (multiple-value-bind (dx dy dz) (swim :c-t 5d-3 :c-n 5d-3 :time 0.5d0)
    (is (< (max (abs dx) (abs dy) (abs dz)) 1d-9) "сдвиг при K = 1: ~,2E ~,2E ~,2E мм" dx dy dz))
  (let ((dx (swim :time 0.5d0)))
    (is (> (abs dx) 1d-2) "контроль: при K ≈ 1,6 та же волна сдвигает тело на ~,4F мм" dx)))

(test reversing-the-wave-reverses-the-motion
  (let ((forward (swim :direction 1d0))
        (backward (swim :direction -1d0)))
    (is (minusp forward) "волна от головы к хвосту — движение к голове (−x): ~,4F мм" forward)
    (is (plusp backward) "обратная волна — к хвосту: ~,4F мм" backward)
    (is (< 0.5d0 (/ (abs backward) (abs forward)) 2d0) "скорости одного порядка")))

(test tension-wave-moves-forward-in-water-and-on-agar
  (let ((water (swim :time 0.75d0))
        (agar (swim :params "agar-a-v1" :frequency 0.5d0 :wavelength 0.65d0 :time 3d0 :h 1d-3)))
    (is (< water -0.02d0) "вода: ~,4F мм за 0,75 с" water)
    (is (< agar -0.05d0) "агар: ~,4F мм за 3 с" agar)))

;;; Этап 5: мышцы под генератором ритма уровня 1

(defun gait-of (body &key (time 1.5d0) (h 5d-4) (id "gait") from-time)
  "Записать прогон BODY в cache/test-physics/ и вернуть метрики с момента
FROM-TIME (по умолчанию — вторая половина прогона)."
  (let* ((dir (worm.oracle:record-body-run
               body :time time :h h :every 0.01d0 :id id
                    :root (merge-pathnames "cache/test-physics/" (worm.core:project-root))))
         (run (worm.analysis:load-run dir))
         (n (length (worm.analysis:run-data-times run))))
    (worm.analysis:gait-metrics
     run :from (if from-time
                   (position-if (lambda (x) (>= x from-time)) (worm.analysis:run-data-times run))
                   (floor n 2)))))

(test muscle-wave-swims-like-the-tension-wave
  ;; Та же волна (2 Гц, 1,5 L), что у кинематического натяжения этапа 3,
  ;; но через кальций, функцию Хилла, f_L и f_V.
  ;; Натяжение 15 мкН — порядка силы мышц; при 20 мкН без ослабления головы
  ;; кинематика сминает крошечный головной отсек.
  (let ((tension (gait-of (worm.oracle:build-body :params "water-v1" :amplitude 15d0 :frequency 2d0
                                                  :wavelength 1.5d0 :ramp 0.5d0)
                          :id "tension" :time 2.5d0 :from-time 1d0))
        (muscles (gait-of (worm.oracle:build-body :params "water-v1" :muscles t :frequency 2d0
                                                  :wavelength 1.5d0 :ramp 0.5d0)
                          :id "muscles" :time 2.5d0 :from-time 1d0)))
    (is (< (abs (- (getf muscles :frequency) 2d0)) 0.02d0) "частота ~,3F Гц" (getf muscles :frequency))
    (is (< (abs (- (getf muscles :wavelength) (getf tension :wavelength))) (* 0.15d0 (getf tension :wavelength)))
        "длина волны ~,3F L против ~,3F L у натяжения" (getf muscles :wavelength) (getf tension :wavelength))
    (is (plusp (getf muscles :speed)) "плывёт вперёд: ~,4F мм/с" (getf muscles :speed))
    (is (< 0.5d0 (/ (getf muscles :speed) (getf tension :speed)) 2d0)
        "скорость ~,4F мм/с против ~,4F у натяжения" (getf muscles :speed) (getf tension :speed))
    (is (< 2d0 (getf muscles :amplitude) 6d0) "κ·L = ~,2F" (getf muscles :amplitude))))

(test calibrated-muscles-match-swimming-kinematics
  ;; Набор water-v1 как есть: 1,76 Гц, 1,54 L; F_max подобрана по κ·L = 2,9
  ;; (Fang-Yen et al. 2010).
  (let ((m (gait-of (worm.oracle:build-body :params "water-v1" :muscles t :ramp 0.5d0)
                    :time 3d0 :id "water-calibrated")))
    (is (< (abs (- (getf m :frequency) 1.76d0)) 0.02d0) "частота ~,3F Гц" (getf m :frequency))
    (is (< (abs (- (getf m :wavelength) 1.54d0)) 0.08d0) "длина волны ~,3F L" (getf m :wavelength))
    (is (< (abs (- (getf m :amplitude) 2.9d0)) 0.3d0) "κ·L = ~,2F" (getf m :amplitude))
    (is (plusp (getf m :speed)) "вперёд: ~,4F мм/с" (getf m :speed))))

(test muscles-crawl-forward-on-agar
  (let ((m (gait-of (worm.oracle:build-body :params "agar-a-v1" :muscles t :ramp 2d0)
                    :time 10d0 :h 1d-3 :id "agar-calibrated" :from-time 3d0)))
    (is (< (abs (- (getf m :frequency) 0.30d0)) 0.01d0) "частота ~,3F Гц" (getf m :frequency))
    (is (< (abs (- (getf m :wavelength) 0.65d0)) 0.1d0) "длина волны ~,3F L" (getf m :wavelength))
    (is (< 0.05d0 (getf m :speed) 0.3d0) "ползёт вперёд: ~,4F мм/с" (getf m :speed))))

(test prestress-keeps-the-section-round-on-agar
  ;; Без p₀ мениск сплющивал сечение лежащего тела на 16–19% по высоте;
  ;; с p₀ = 30 кПа — меньше чем на 5%.
  (let* ((body (worm.oracle:build-body :params "agar-a-v1"))
         (it (run-body body :time 0.3d0 :every 600))
         (y (integrator-y it))
         (zs (loop for k below 8 collect (aref y (+ (* 3 (+ (* 25 8) k)) 2))))
         (height (- (reduce #'max zs) (reduce #'min zs)))
         (round-height (* 2 (aref (worm.topology:tube-ring-radius (worm.oracle:body-tube body)) 25)
                          (cos (/ pi 8)))))
    (is (< (- 1 (/ height round-height)) 0.05d0) "высота сечения ~,1F мкм из ~,1F"
        (* 1000 height) (* 1000 round-height))))

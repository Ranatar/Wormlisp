;;;; tests/track.lisp — метрики походки по средней линии WCON (этап 14).
;;;;
;;;; Сначала код проверяется на синтетическом треке с известными частотой,
;;;; длиной волны и скоростью, с шумом сегментации и пропусками; затем на
;;;; отрывке трека живого червя (reference/schafer-n2-agar) закрепляются его
;;;; метрики.

(in-package #:worm.tests)

(in-suite analysis)

(defun synthetic-wcon (&key (f 0.4d0) (lambda-l 0.6d0) (a 0.04d0) (v 0.1d0) (noise 2d-3)
                            (fps 12.5d0) (seconds 60d0) (points 49) (drop-every 7) (gap '(30d0 31d0)))
  "Объект WCON: червь длиной 1 мм по оси x, голова — первая точка, ползёт
к −x со скоростью V; изгиб y = A·sin 2π(u/λ − f·t), где u — расстояние от
головы вдоль оси. Шум — детерминированный, до NOISE мм; каждый DROP-EVERY-й
кадр — пропуск, на отрезке GAP пропусков сплошь."
  (let* ((n (round (* fps seconds)))
         (state 12345)
         (rnd (lambda () (setf state (mod (+ (* state 1103515245) 12345) 2147483648))
                (- (* 2d0 (/ state 2147483648d0)) 1d0)))
         (ts (make-array n)) (xs (make-array n)) (ys (make-array n)))
    (dotimes (k n)
      (let ((time (/ k fps)))
        (setf (aref ts k) time)
        (if (or (zerop (mod (1+ k) drop-every)) (<= (first gap) time (second gap)))
            (setf (aref xs k) (make-array points :initial-element 'null)
                  (aref ys k) (make-array points :initial-element 'null))
            (let ((x (make-array points)) (y (make-array points)))
              (dotimes (i points)
                (let ((u (/ i (1- points) 1d0)))
                  (setf (aref x i) (+ (- u (* v time)) (* noise (funcall rnd)))
                        (aref y i) (+ (* a (sin (* 2 pi (- (/ u lambda-l) (* f time))))) (* noise (funcall rnd))))))
              (setf (aref xs k) x (aref ys k) y)))))
    (worm.io:json-object "units" (worm.io:json-object "t" "s" "x" "mm" "y" "mm")
                         "data" (vector (worm.io:json-object "id" "1" "head" "L" "t" ts "x" xs "y" ys)))))

(test track-metrics-recover-a-synthetic-wave
  (let* ((w (synthetic-wcon))
         (tr (worm.analysis:track-from-wcon w))
         (g (worm.analysis:track-gait tr :window 10d0 :smooth 0.1d0)))
    (is (null (wcon-errors w)) "синтетический трек — правильный WCON: ~{~A~^; ~}" (wcon-errors w))
    (is (= 2 (length (worm.analysis:track-segments tr))) "длинный пропуск делит трек на два отрезка")
    (flet ((mid (key) (second (getf g key))))
      (is (< (abs (- (mid :frequency) 0.4d0)) 0.012d0) "частота ~,3F, задана 0,4" (mid :frequency))
      (is (< (abs (- (mid :wavelength) 0.6d0)) 0.03d0) "длина волны ~,3F L, задана 0,6" (mid :wavelength))
      ;; скорость — в мм/с, длина трека по дуге чуть больше 1 мм
      (is (< (abs (- (mid :speed) 0.1d0)) 0.005d0) "скорость ~,4F мм/с, задана 0,1" (mid :speed))
      ;; пик кривизны A·(2π/λ)² = 4,39 1/мм; хорды сглаживания его немного занижают
      (let ((expected (* 0.04d0 (expt (/ (* 2 pi) 0.6d0) 2) (worm.analysis:track-length tr))))
        (is (< 0.85d0 (/ (mid :amplitude) expected) 1.02d0) "κ·L ~,2F, ожидается около ~,2F" (mid :amplitude) expected)))))

(test track-curvature-needs-smoothing-against-segmentation-noise
  ;; шум 2 мкм на соседних точках (шаг 21 мкм) — та же картина, что у трека
  ;; живого червя: без сглаживания частота уходит вверх
  (let* ((tr (worm.analysis:track-from-wcon (synthetic-wcon :noise 4d-3)))
         (raw (second (getf (worm.analysis:track-gait tr :smooth 0d0) :frequency)))
         (smooth (second (getf (worm.analysis:track-gait tr :smooth 0.1d0) :frequency))))
    (is (< (abs (- smooth 0.4d0)) 0.02d0) "со сглаживанием ~,3F Гц" smooth)
    (is (or (null raw) (> raw 0.6d0)) "без сглаживания шум даёт ~A Гц" raw)))

(defun reference-track-path ()
  (merge-pathnames "reference/schafer-n2-agar/track.wcon" (worm.core:project-root)))

(test reference-track-is-valid-wcon
  (let ((w (worm.io:read-json-file (reference-track-path))))
    (is (null (wcon-errors w)) "~{~A~^; ~}" (wcon-errors w))
    (is (search "CC BY 4.0" (gethash "source" (gethash "@Wormlisp" w))) "источник и лицензия записаны")))

(test reference-track-gait
  ;; Числа — сводка трека N2 на агаре при сглаживании 0,1 L, окна 10 с
  ;; (план, этап 14); допуски — на округление и мелкие правки кода, а не на
  ;; разброс червей.
  (let* ((tr (worm.analysis:read-track (reference-track-path)))
         (g (worm.analysis:track-gait tr)))
    (is (= 49 (worm.analysis:track-points tr)))
    (is (< 0.78d0 (worm.analysis:track-length tr) 0.84d0) "длина ~,3F мм" (worm.analysis:track-length tr))
    (is (>= (getf g :forward) 15) "окон движения вперёд ~D" (getf g :forward))
    (flet ((mid (key) (second (getf g key))))
      (is (< 0.28d0 (mid :frequency) 0.36d0) "частота ~,3F Гц (N2 на агаре — около 1/3 Гц)" (mid :frequency))
      (is (< 0.55d0 (mid :wavelength) 0.70d0) "длина волны ~,3F L" (mid :wavelength))
      (is (< 7d0 (mid :amplitude) 9.5d0) "κ·L ~,2F" (mid :amplitude))
      (is (< 0.07d0 (mid :speed) 0.12d0) "скорость ~,3F мм/с" (mid :speed))
      (is (< 0.5d0 (mid :slip) 0.8d0) "доля скорости волны ~,2F" (mid :slip)))))

(test boyle-reference-gait
  ;; Исходная модель Бойла (reference/boyle2012-wormsim, план, этап 15):
  ;; от воды к агару частота падает, длина волны падает, κ·L растёт.
  (flet ((gait (m) (worm.analysis:track-gait
                    (worm.analysis:read-track
                     (merge-pathnames (format nil "reference/boyle2012-wormsim/medium-~A.wcon" m)
                                      (worm.core:project-root)))
                    :from 5d0 :window 5d0 :min-speed 0d0)))
    (let ((water (gait "0")) (agar (gait "1")))
      (flet ((mid (g key) (second (getf g key))))
        (is (< 2.1d0 (mid water :frequency) 2.35d0) "вода: ~,3F Гц" (mid water :frequency))
        (is (< 1.05d0 (mid water :wavelength) 1.3d0) "вода: λ ~,3F L" (mid water :wavelength))
        (is (< 2.8d0 (mid water :amplitude) 3.6d0) "вода: κ·L ~,2F" (mid water :amplitude))
        (is (< 0.36d0 (mid agar :frequency) 0.44d0) "агар: ~,3F Гц" (mid agar :frequency))
        (is (< 0.56d0 (mid agar :wavelength) 0.68d0) "агар: λ ~,3F L" (mid agar :wavelength))
        (is (< 7d0 (mid agar :amplitude) 8.6d0) "агар: κ·L ~,2F" (mid agar :amplitude))))))

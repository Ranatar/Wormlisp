;;;; src/analysis/track.lisp — метрики походки по средней линии из WCON (этап 14).
;;;;
;;;; Трек живого червя меряется тем же кодом, что прогон модели: из средней
;;;; линии строятся матрицы кривизны, центра и направления тела, и по ним
;;;; считает gait-metrics. Отличия трека от кадров модели и как они снимаются:
;;;;
;;;;   - Пропуски. Трекер не сегментирует часть кадров (null в x, y).
;;;;     Пропуск не длиннее max-gap секунд (0,25 с, как у Yemini et al. 2013)
;;;;     заполняется линейной интерполяцией по времени, более длинный делит
;;;;     трек на отрезки; метрики считаются внутри отрезков.
;;;;   - Шум сегментации. Кривизна по соседним точкам средней линии из 49
;;;;     точек — в основном шум: на треке N2 она даёт частоту около 1,9 Гц
;;;;     вместо 1/3 Гц. Поэтому касательная берётся по точкам на расстоянии
;;;;     smooth·(P−1) по обе стороны (smooth — доля длины тела), а кривизна —
;;;;     разность углов, делённая на длину дуги между ними. У Yemini et al.
;;;;     сглаживает усреднение углов изгиба по участкам тела, с тем же
;;;;     смыслом.
;;;;   - Состояния движения. Живой червь останавливается, пятится, делает
;;;;     повороты. Метрики считаются по окнам window секунд, и в сводку идут
;;;;     окна движения вперёд: скорость не меньше min-speed длины тела в
;;;;     секунду (у Yemini et al. порог движения — 5% длины в секунду).
;;;;
;;;; Знак кривизны — как у плоской кривой в осях трека; для частоты, длины
;;;; волны и амплитуд он не важен.

(in-package #:worm.analysis)

(defstruct (track (:copier nil))
  meta          ; объект WCON целиком, без data
  times         ; dvec времён кадров, с
  xs ys         ; simple-vector кадров: dvec точек в мм или NIL — пропуск
  points        ; число точек средней линии
  length)       ; медиана длины средней линии, мм

(defun length-unit-scale (unit)
  "Множитель единицы длины WCON к миллиметрам."
  (cond ((member unit '("mm" "millimeters" "millimetres") :test #'string-equal) 1d0)
        ((member unit '("um" "micrometers" "micrometres" "microns") :test #'string-equal) 1d-3)
        ((member unit '("m" "meters" "metres") :test #'string-equal) 1d3)
        (t (error "единица длины WCON ~S неизвестна" unit))))

(defun spine-length (x y)
  (loop for i from 1 below (length x)
        sum (sqrt (+ (sq (- (aref x i) (aref x (1- i)))) (sq (- (aref y i) (aref y (1- i))))))))

(defun read-track (path &key (id nil))
  "Трек одной особи из файла WCON: запись data с идентификатором ID (по
умолчанию первая)."
  (track-from-wcon (worm.io:read-json-file path) :id id :where path))

(defun track-from-wcon (w &key (id nil) (where "WCON"))
  "Трек одной особи из объекта WCON W (хэш-таблица, как её читает jzon)."
  (let* ((path where)
         (units (gethash "units" w))
         (scale (length-unit-scale (gethash "x" units)))
         (data (gethash "data" w))
         (data (if (hash-table-p data) (vector data) data))
         (record (if id
                     (or (find id data :key (lambda (r) (gethash "id" r)) :test #'equal)
                         (error "в ~A нет особи ~S" path id))
                     (aref data 0)))
         (times (dvec-from (gethash "t" record)))
         (frame (lambda (v) (and (vectorp v) (plusp (length v)) (every #'realp v)
                                 (map 'dvec (lambda (c) (* scale c)) v))))
         (xs (map 'simple-vector frame (gethash "x" record)))
         (ys (map 'simple-vector frame (gethash "y" record)))
         (first-good (position-if #'identity xs)))
    (unless first-good (error "в ~A нет ни одного кадра средней линии" path))
    (unless (member (gethash "head" record) '(nil "L") :test #'equal)
      (error "голова не в первой точке (head = ~S): такой трек пока не читается" (gethash "head" record)))
    (dotimes (k (length xs))
      (unless (and (aref xs k) (aref ys k)) (setf (aref xs k) nil (aref ys k) nil)))
    (make-track :meta (let ((m (make-hash-table :test 'equal)))
                        (maphash (lambda (k v) (unless (equal k "data") (setf (gethash k m) v))) w)
                        m) :times times :xs xs :ys ys
                :points (length (aref xs first-good))
                :length (median (loop for k below (length xs)
                                      when (aref xs k) collect (spine-length (aref xs k) (aref ys k)))))))

(defun track-frame-interval (track)
  "Медиана шага времени между кадрами трека, с."
  (let ((times (track-times track)))
    (median (loop for k from 1 below (length times) collect (- (aref times k) (aref times (1- k)))))))

(defun track-segments (track &key (max-gap 0.25d0))
  "Отрезки трека без пропусков длиннее MAX-GAP секунд (пропуск — время
между кадрами с данными за вычетом шага кадров): список (from . to)
номеров кадров; первый и последний кадр отрезка — с данными."
  (let* ((xs (track-xs track)) (times (track-times track)) (segments '()) (start nil) (last nil)
         (frame (track-frame-interval track)))
    (dotimes (k (length xs))
      (when (aref xs k)
        (cond ((null start) (setf start k))
              ((> (- (aref times k) (aref times last) frame) max-gap)
               (push (cons start (1+ last)) segments)
               (setf start k)))
        (setf last k)))
    (when start (push (cons start (1+ last)) segments))
    (nreverse segments)))

(defun filled-frames (track from to)
  "Кадры [FROM, TO) с пропусками, заполненными линейной интерполяцией по
времени: (values xs ys) — simple-vector dvec."
  (let ((xs (subseq (track-xs track) from to)) (ys (subseq (track-ys track) from to))
        (times (track-times track)))
    (loop for k from 0 below (length xs)
          unless (aref xs k)
            do (let* ((a (position-if #'identity xs :end k :from-end t))
                      (b (position-if #'identity xs :start k))
                      (ta (aref times (+ from a))) (tb (aref times (+ from b)))
                      (u (/ (- (aref times (+ from k)) ta) (- tb ta))))
                 (flet ((mix (va vb) (map 'dvec (lambda (p q) (+ (* (- 1 u) p) (* u q))) va vb)))
                   (setf (aref xs k) (mix (aref xs a) (aref xs b))
                         (aref ys k) (mix (aref ys a) (aref ys b))))))
    (values xs ys)))

(defun spine-curvatures (x y smooth out)
  "Знаковая кривизна плоской средней линии (X, Y) в каждой точке, 1/мм:
разность направлений хорд к точкам на SMOOTH·(P−1) позади и впереди,
делённая на полусумму длин хорд; у концов хорды укорачиваются. SMOOTH 0 —
соседние точки."
  (let* ((p (length x)) (h (max 1 (round (* smooth (1- p))))))
    (dotimes (i p out)
      (let* ((a (max 0 (- i h))) (b (min (1- p) (+ i h))))
        (setf (aref out i)
              (if (or (= a i) (= b i))
                  0d0
                  (let* ((ax (- (aref x i) (aref x a))) (ay (- (aref y i) (aref y a)))
                         (bx (- (aref x b) (aref x i))) (by (- (aref y b) (aref y i)))
                         (turn (atan (- (* ax by) (* ay bx)) (+ (* ax bx) (* ay by))))
                         (arc (* 0.5d0 (+ (sqrt (+ (sq ax) (sq ay))) (sqrt (+ (sq bx) (sq by)))))))
                    (if (plusp arc) (/ turn arc) 0d0))))))))

(defun spine-series (xs ys smooth)
  "Матрицы кадры × точки кривизны, кадры × 3 центра и кадры × 3 направления
тела (от задней четверти к передней) по кадрам средней линии XS, YS."
  (let* ((n (length xs)) (p (length (aref xs 0))) (q (max 1 (floor p 4)))
         (curv (make-dmat n p)) (cents (make-dmat n 3)) (heads (make-dmat n 3))
         (row (make-dvec p)))
    (dotimes (k n (values curv cents heads))
      (let ((x (aref xs k)) (y (aref ys k)))
        (spine-curvatures x y smooth row)
        (dotimes (i p) (setf (aref curv k i) (aref row i)))
        (setf (aref cents k 0) (/ (reduce #'+ x) p) (aref cents k 1) (/ (reduce #'+ y) p))
        (let* ((hx (- (/ (loop for i below q sum (aref x i)) q) (/ (loop for i from (- p q) below p sum (aref x i)) q)))
               (hy (- (/ (loop for i below q sum (aref y i)) q) (/ (loop for i from (- p q) below p sum (aref y i)) q)))
               (norm (sqrt (+ (sq hx) (sq hy)))))
          (when (plusp norm) (setf (aref heads k 0) (/ hx norm) (aref heads k 1) (/ hy norm))))))))

(defun quartiles (values)
  "(первый квартиль медиана третий квартиль) списка чисел."
  (let* ((s (sort (copy-list values) #'<)) (n (length s)))
    (flet ((at (q) (let* ((pos (* q (1- n))) (i (floor pos)) (f (- pos i)))
                     (if (< (1+ i) n) (+ (* (- 1 f) (nth i s)) (* f (nth (1+ i) s))) (nth i s)))))
      (and (plusp n) (list (at 0.25d0) (at 0.5d0) (at 0.75d0))))))

(defun track-windows (track &key (window 10d0) (smooth 0.1d0) (max-gap 0.25d0))
  "Метрики походки трека по окнам WINDOW секунд внутри отрезков: список
(начало конец . plist gait-metrics)."
  (loop for (from . to) in (track-segments track :max-gap max-gap)
        nconc (multiple-value-bind (xs ys) (filled-frames track from to)
                (multiple-value-bind (curv cents heads) (spine-series xs ys smooth)
                  (let* ((times (subseq (track-times track) from to))
                         (data (make-run-data :times times :rings (track-points track)
                                              :length (track-length track)))
                         (n (length times)))
                    (loop with start = 0
                          for t0 = (aref times start)
                          for end = (position-if (lambda (x) (>= x (+ t0 window))) times :start start)
                          while end
                          collect (list* t0 (aref times (1- end))
                                         (gait-metrics data :from start :to end :curvature curv
                                                            :centroids cents :headings heads))
                          do (setf start end)
                          while (< start (1- n))))))))

(defun track-gait (track &key (window 10d0) (smooth 0.1d0) (min-speed 0.05d0) (max-gap 0.25d0))
  "Сводка походки трека по окнам движения вперёд (скорость не меньше
MIN-SPEED длины тела в секунду, частота и длина волны определены): plist
:windows (число окон) :forward (окон вперёд) и для :speed :frequency
:wavelength :amplitude :wave-speed :slip — квартили (q1 медиана q3); плюс
:curvature-profile — медианы по пятым частям тела и :length."
  (let* ((all (track-windows track :window window :smooth smooth :max-gap max-gap))
         (len (track-length track))
         (forward (remove-if-not (lambda (w) (let ((m (cddr w)))
                                               (and (getf m :speed) (>= (getf m :speed) (* min-speed len))
                                                    (getf m :frequency) (getf m :wavelength))))
                                 all)))
    (append (list :windows (length all) :forward (length forward) :length len)
            (loop for key in '(:speed :frequency :wavelength :amplitude :wave-speed :slip)
                  append (list key (quartiles (loop for w in forward for v = (getf (cddr w) key) when v collect v))))
            (list :curvature-profile
                  (and forward
                       (coerce (loop for i below 5
                                     collect (median (loop for w in forward
                                                           collect (aref (getf (cddr w) :curvature-profile) i))))
                               'vector))))))

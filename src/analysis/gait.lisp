;;;; src/analysis/gait.lisp — метрики походки по кадрам прогона.
;;;;
;;;; Кривизна — знаковая дорсо-вентральная, как у наблюдаемой модели
;;;; (worm.topology:ring-curvatures): положительна, когда центр кривизны
;;;; лежит с дорсальной стороны.
;;;;
;;;; Частота — по моментам перехода кривизны через ноль снизу вверх в
;;;; кольцах средней трети тела (с гистерезисом в десятую долю размаха). Длина волны — через скорость волны: волна
;;;; проходит между кольцами на 0,35 L и 0,55 L с задержкой τ, скорость
;;;; c = 0,2 L / τ, длина λ = c / f. Скорость тела — проекция смещения
;;;; центра на направление от хвоста к голове, положительна при движении
;;;; вперёд.

(in-package #:worm.analysis)

(defstruct (run-data (:copier nil))
  meta frames rings ring-nodes length times)

(defun meta-get (meta &rest keys)
  (let ((v meta))
    (dolist (k keys v)
      (setf v (and (hash-table-p v) (gethash k v))))))

(defun load-run (dir)
  "Прочитать meta.json и frames.bin каталога прогона DIR."
  (let* ((meta (worm.io:read-json-file (worm.io:run-file dir "meta.json")))
         (frames (worm.io:read-frames-file (worm.io:run-file dir "frames.bin")))
         (rings (meta-get meta "topology" "rings"))
         (ring-nodes (meta-get meta "topology" "ringNodes"))
         (times (make-dvec (worm.io:frames-file-count frames))))
    (unless (and rings ring-nodes (= (* rings ring-nodes) (worm.io:frames-file-nodes frames)))
      (error "meta.json не описывает топологию кадров ~A" dir))
    (dotimes (k (length times))
      (setf (aref times k) (worm.io:frame-time (worm.io:frame-ref frames k))))
    (make-run-data :meta meta :frames frames :rings rings :ring-nodes ring-nodes
                   :length (float (or (meta-get meta "topology" "length") 1d0) 0d0)
                   :times times)))

(defun frame-dvec (run k)
  (dvec-from (worm.io:frame-coords (worm.io:frame-ref (run-data-frames run) k))))

(defun curvature-series (run)
  "Матрица кадры × кольца знаковой кривизны, 1/мм."
  (let* ((n (worm.io:frames-file-count (run-data-frames run)))
         (rings (run-data-rings run))
         (out (make-dmat n rings))
         (centers (make-dvec (* 3 rings)))
         (row (make-dvec rings)))
    (dotimes (k n out)
      (worm.topology:ring-curvatures (frame-dvec run k) rings (run-data-ring-nodes run) centers row)
      (dotimes (i rings) (setf (aref out k i) (aref row i))))))

(defun centroid-series (run)
  "Матрица кадры × 3 — среднее центров колец."
  (let* ((n (worm.io:frames-file-count (run-data-frames run)))
         (rings (run-data-rings run))
         (centers (make-dvec (* 3 rings)))
         (out (make-dmat n 3)))
    (dotimes (k n out)
      (worm.topology:ring-centers (frame-dvec run k) rings (run-data-ring-nodes run) centers)
      (dotimes (c 3)
        (setf (aref out k c) (/ (loop for i below rings sum (aref centers (+ (* 3 i) c))) rings))))))

(defun heading-series (run)
  "Матрица кадры × 3 — единичное направление от центра задней четверти к
центру передней четверти тела."
  (let* ((n (worm.io:frames-file-count (run-data-frames run)))
         (rings (run-data-rings run))
         (q (max 1 (floor rings 4)))
         (centers (make-dvec (* 3 rings)))
         (out (make-dmat n 3)))
    (dotimes (k n out)
      (worm.topology:ring-centers (frame-dvec run k) rings (run-data-ring-nodes run) centers)
      (let ((v (loop for c below 3
                     collect (- (/ (loop for i below q sum (aref centers (+ (* 3 i) c))) q)
                                (/ (loop for i from (- rings q) below rings sum (aref centers (+ (* 3 i) c))) q)))))
        (let ((norm (sqrt (reduce #'+ (mapcar #'sq v)))))
          (loop for c below 3 for x in v do (setf (aref out k c) (/ x norm))))))))

(defparameter +crossing-floor+ 1d-2
  "Наименьший порог гистерезиса, 1/мм: сотая доля обратного миллиметра —
на порядки ниже кривизны походки (единицы 1/мм) и выше шума float32.")

(defun upward-crossings (series column times from to)
  "Моменты перехода через ноль снизу вверх в столбце COLUMN на [FROM, TO),
с линейной интерполяцией. С гистерезисом: переход засчитывается, только
если перед ним значение опускалось ниже −θ, где θ — десятая доля размаха
столбца в окне, но не меньше +crossing-floor+; иначе дрожание малой
кривизны около нуля (разгон, шум одинарной точности) дало бы ложные периоды."
  (let ((theta (max +crossing-floor+
                    (* 0.1d0 (loop for k from from below to maximize (abs (aref series k column))))))
        (armed nil))
    (loop for k from (1+ from) below to
          for a = (aref series (1- k) column)
          for b = (aref series k column)
          do (when (< a (- theta)) (setf armed t))
          when (and armed (< a 0d0) (>= b 0d0))
            collect (let ((ta (aref times (1- k))) (tb (aref times k)))
                      (setf armed nil)
                      (+ ta (* (- tb ta) (/ (- a) (- b a))))))))

(defun median (list)
  (let* ((sorted (sort (copy-list list) #'<)) (n (length sorted)))
    (cond ((zerop n) nil)
          ((oddp n) (nth (floor n 2) sorted))
          (t (* 0.5d0 (+ (nth (1- (floor n 2)) sorted) (nth (floor n 2) sorted)))))))

(defun gait-frequency (curv times from to rings)
  "Частота волны по кольцам средней трети, Гц; NIL, если периодов меньше одного."
  (median
   (loop for i from (floor rings 3) below (floor (* 2 rings) 3)
         for ups = (upward-crossings curv i times from to)
         when (>= (length ups) 2)
           collect (/ (1- (length ups)) (- (car (last ups)) (first ups))))))

(defun gait-wavelength (curv times from to rings frequency)
  "Длина волны в долях L; NIL, если её не определить."
  (when frequency
    (let* ((i0 (round (* 0.35d0 (1- rings))))
           (i1 (round (* 0.55d0 (1- rings))))
           (ds (/ (float (- i1 i0) 0d0) (1- rings)))
           (period (/ 1d0 frequency))
           (front (upward-crossings curv i0 times from to))
           (back (upward-crossings curv i1 times from to))
           (lags (loop for t0 in front
                       for t1 = (find-if (lambda (x) (> x t0)) back)
                       when (and t1 (< (- t1 t0) period))
                         collect (- t1 t0))))
      (let ((lag (median lags)))
        (and lag (plusp lag) (/ (* (/ ds lag)) frequency))))))

(defun gait-speed (centroids headings times from to)
  "Скорость вперёд, мм/с: смещение центра за окно на среднее направление тела."
  (let ((dt (- (aref times (1- to)) (aref times from))))
    (when (plusp dt)
      (let ((dx (loop for c below 3 collect (- (aref centroids (1- to) c) (aref centroids from c))))
            (h (loop for c below 3 collect (/ (loop for k from from below to sum (aref headings k c))
                                               (- to from)))))
        (/ (reduce #'+ (mapcar #'* dx h)) (sqrt (reduce #'+ (mapcar #'sq h))) dt)))))

(defun gait-amplitude (curv from to rings length)
  "Средняя амплитуда кривизны, умноженная на L: пик |κ| каждого кольца
средней трети за окно, усреднённый по этим кольцам (как κ·L у Fang-Yen
et al. 2010)."
  (let ((peaks (loop for i from (floor rings 3) below (floor (* 2 rings) 3)
                     collect (loop for k from from below to maximize (abs (aref curv k i))))))
    (and peaks (> to from) (* length (/ (reduce #'+ peaks) (length peaks))))))

(defun gait-metrics (run &key (from 0) (to (worm.io:frames-file-count (run-data-frames run)))
                              curvature centroids headings)
  "Метрики походки на кадрах [FROM, TO): plist :speed :frequency :wavelength :amplitude."
  (let* ((curv (or curvature (curvature-series run)))
         (cents (or centroids (centroid-series run)))
         (heads (or headings (heading-series run)))
         (times (run-data-times run))
         (rings (run-data-rings run))
         (f (gait-frequency curv times from to rings)))
    (list :speed (gait-speed cents heads times from to)
          :frequency f
          :wavelength (gait-wavelength curv times from to rings f)
          :amplitude (gait-amplitude curv from to rings (run-data-length run)))))

(defun metrics-windows (run window)
  "Метрики по окнам длиной WINDOW секунд: список (начало конец . plist)."
  (let* ((curv (curvature-series run)) (cents (centroid-series run)) (heads (heading-series run))
         (times (run-data-times run)) (n (length times)))
    (loop with start = 0
          while (< start (1- n))
          for t0 = (aref times start)
          for end = (or (position-if (lambda (x) (>= x (+ t0 window))) times :start start) n)
          when (> (- end start) 2)
            collect (list* t0 (aref times (1- (min end n)))
                           (gait-metrics run :from start :to (min end n) :curvature curv
                                             :centroids cents :headings heads))
          do (setf start end))))

(defun write-metrics-csv (path windows)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-line "t_start_s,t_end_s,speed_mm_s,frequency_hz,wavelength_L,curvature_amplitude_kL" out)
    (dolist (w windows)
      (destructuring-bind (t0 t1 &key speed frequency wavelength amplitude) w
        (format out "~,4F,~,4F,~@[~,5F~],~@[~,4F~],~@[~,4F~],~@[~,3F~]~%"
                t0 t1 speed frequency wavelength amplitude)))
    path))

;;; Сводка развёртки (этап 11)

(defun run-gait (dir &key (from 0d0))
  "Метрики походки прогона в каталоге DIR с момента FROM, с: plist или NIL."
  (let* ((run (load-run dir))
         (times (run-data-times run))
         (start (or (position-if (lambda (x) (>= x from)) times) 0)))
    (when (> (- (length times) start) 2)
      (gait-metrics run :from start))))

(defun write-run-metrics (dir &key (window 1d0))
  "metrics.csv каталога прогона по окнам WINDOW секунд."
  (write-metrics-csv (worm.io:run-file dir "metrics.csv") (metrics-windows (load-run dir) window)))

(defun write-sweep-summary (path rows)
  "summary.csv развёртки. ROWS — plist (:id :status :point ((имя значение) …)
:metrics plist или NIL), у всех строк одни и те же имена точек."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create
                            :external-format :utf-8)
    (let ((names (mapcar #'first (getf (first rows) :point))))
      (format out "id,status~{,~(~A~)~},frequency_hz,wavelength_L,curvature_amplitude_kL,speed_mm_s~%" names)
      (dolist (r rows)
        (let ((m (getf r :metrics)))
          (format out "~A,~A~{,~F~},~@[~F~],~@[~F~],~@[~F~],~@[~F~]~%"
                  (getf r :id) (getf r :status) (mapcar #'second (getf r :point))
                  (getf m :frequency) (getf m :wavelength) (getf m :amplitude) (getf m :speed))))))
  path)

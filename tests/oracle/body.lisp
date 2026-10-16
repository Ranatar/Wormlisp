;;;; tests/oracle/body.lisp — модель тела и среды из ручных ядер.
;;;;
;;;; Жёсткости рёбер выводятся из изгибной жёсткости EI(s) (параметры,
;;;; «Кутикула и ткани»): EI постоянна от головы до 2/3 длины и линейно
;;;; падает к хвосту до 1 − tail-softening (Backholm et al. 2013), продольное
;;;; ребро k_c(s) = 2·EI(s)/(K·R(s)²·Δs) — к тонким концам рёбра жёстче;
;;;; окружные рёбра, диагонали и диаметры — доли k_c; вязкость η = τ_c·k.
;;;; Привод — кинематическая мышца: натяжение 95 клеток задано бегущей
;;;; волной (дизайн, уровень 1) и рассеивается на продольные рёбра по
;;;; привязке этапа 1. Настоящие мышцы с F_max по клеткам — этап 5.
;;;; Режим antagonist = 1 не обрезает отрицательную часть волны: тогда
;;;; одна сторона тянет, другая толкает с той же силой, и к сечению
;;;; приложен чистый изгибающий момент без продольной силы.
;;;;
;;;; Скалярные параметры, которые можно менять на ходу, лежат в векторе p
;;;; (+P-LAYOUT+); поэлементные величины — в замыканиях модели.

(in-package #:worm.oracle)

(defparameter +p-layout+
  '(:c-t :c-n :bulk-modulus :meniscus-force :contact-stiffness
    :amplitude :frequency :wavelength :direction :ramp :phase :antagonist)
  "Порядок скалярных параметров в векторе p ручной модели.")

(defparameter +all-kernels+
  '(:long-edge :circ-edge :diagonal :diametral :pressure :drag :contact :tension))

(defstruct (body (:copier nil))
  model tube anatomy binding params pvec y0 medium kernels
  long-stiffness rest-volume)

(defun set-body-param (vector name value)
  "Записать параметр NAME в вектор p ручной модели (или интегратора)."
  (setf (aref vector (or (position name +p-layout+)
                         (error "нет параметра ~S в ручной модели" name)))
        (float value 0d0)))

;;; Жёсткости

(defun tail-factor (s softening)
  "Множитель EI: 1 до 2/3 длины, затем линейно до 1 − SOFTENING к хвосту."
  (- 1d0 (* softening (max 0d0 (min 1d0 (/ (- s 2/3) 1/3))))))

(defun edge-stiffness (tube family bending-ei tail-softening ratios)
  "Жёсткость каждого ребра семейства FAMILY."
  (let* ((kk (worm.topology:tube-ring-nodes tube))
         (rings (worm.topology:tube-rings tube))
         (radius (worm.topology:tube-ring-radius tube))
         (frac (worm.topology:tube-ring-fractions tube))
         (ds (/ (worm.topology:tube-length tube) (1- rings)))
         (out (make-dvec (worm.topology:family-size tube family))))
    (flet ((kc (r s)
             ;; EI(s) = EI_mid·хвост(s); k_c = 2·EI/(K·R²·Δs)
             (/ (* 2 bending-ei (tail-factor s tail-softening))
                (* kk r r ds))))
      (ecase family
        ((:long-edge :diagonal)
         (let ((per-ring (if (eq family :long-edge) kk (* 2 kk)))
               (ratio (if (eq family :long-edge) 1d0 (getf ratios :shear))))
           (dotimes (i (1- rings))
             (let ((k (* ratio (kc (* 0.5d0 (+ (aref radius i) (aref radius (1+ i))))
                                   (* 0.5d0 (+ (aref frac i) (aref frac (1+ i))))))))
               (dotimes (j per-ring) (setf (aref out (+ (* i per-ring) j)) k))))))
        ((:circ-edge :diametral)
         (let ((per-ring (if (eq family :circ-edge) kk (/ kk 2)))
               (ratio (getf ratios (if (eq family :circ-edge) :circ :diam))))
           (dotimes (i rings)
             (let ((k (* ratio (kc (aref radius i) (aref frac i)))))
               (dotimes (j per-ring) (setf (aref out (+ (* i per-ring) j)) k))))))))
    out))

;;; Размещение

(defun arc-coords (tube curvature)
  "Координаты трубки, изогнутой дугой постоянной кривизны CURVATURE (1/мм)
в дорсо-вентральной плоскости xy; изгиб к дорсальной стороне при CURVATURE > 0."
  (let* ((kk (worm.topology:tube-ring-nodes tube))
         (rings (worm.topology:tube-rings tube))
         (rest (worm.topology:tube-rest-coords tube))
         (s-vec (worm.topology:tube-ring-s tube))
         (out (make-dvec (length rest))))
    (dotimes (i rings out)
      (let* ((s (aref s-vec i))
             (phi (* curvature s))
             (cx (if (zerop curvature) s (/ (sin phi) curvature)))
             (cy (if (zerop curvature) 0d0 (/ (- 1d0 (cos phi)) curvature))))
        (dotimes (k kk)
          (let* ((p (* 3 (+ (* i kk) k)))
                 (oy (aref rest (+ p 1))) (oz (aref rest (+ p 2))))
            ;; смещение узла от оси поворачивается вместе с касательной
            (setf (aref out p) (- cx (* (sin phi) oy))
                  (aref out (+ p 1)) (+ cy (* (cos phi) oy))
                  (aref out (+ p 2)) oz)))))))

(defun place-on-surface (coords tube)
  "Положить тело на бок на плоскость z = 0: каждое кольцо поднять так, чтобы
нижние узлы касались плоскости. Дорсо-вентральная ось остаётся в плоскости."
  (let* ((kk (worm.topology:tube-ring-nodes tube))
         (rings (worm.topology:tube-rings tube))
         (out (copy-seq coords)))
    (dotimes (i rings out)
      (let ((lowest (loop for k below kk minimize (aref coords (+ (* 3 (+ (* i kk) k)) 2)))))
        (dotimes (k kk)
          (decf (aref out (+ (* 3 (+ (* i kk) k)) 2)) lowest))))))

;;; Наблюдаемые: кривизна колец — worm.topology:ring-curvatures

(defun weighted-centroid (y tube)
  "Центр тела с весами Δs колец — величина, которую при K = 1 внутренние
силы сдвинуть не могут. Три значения x, y, z."
  (let* ((kk (worm.topology:tube-ring-nodes tube))
         (rings (worm.topology:tube-rings tube))
         (ds (worm.topology:tube-ring-ds tube))
         (sx 0d0) (sy 0d0) (sz 0d0) (total 0d0))
    (dotimes (i rings)
      (dotimes (k kk)
        (let ((p (* 3 (+ (* i kk) k))) (w (/ (aref ds i) kk)))
          (incf sx (* w (aref y p))) (incf sy (* w (aref y (+ p 1))))
          (incf sz (* w (aref y (+ p 2)))) (incf total w))))
    (values (/ sx total) (/ sy total) (/ sz total))))

(defun volume-ratios (y tube)
  "V/V⁰ каждого отсека."
  (let ((v0 (worm.topology:tube-rest-volume tube))
        (kk (worm.topology:tube-ring-nodes tube)))
    (map 'dvec (lambda (c v) (/ (worm.topology:compartment-volume y c kk) v))
         (loop for c below (length v0) collect c) v0)))

;;; Модель

(defun param (set name) (float (worm.params:param-value set name) 0d0))

(defun build-body (&key (params "water-v1") (rings 50) (ring-nodes 8)
                        (kernels +all-kernels+) (amplitude 0d0) (frequency 1d0)
                        (wavelength 1d0) (direction 1d0) (ramp 0d0) (phase 0d0)
                        antagonist initial-curvature)
  "Собрать ручную модель тела. PARAMS — имя набора параметров; KERNELS —
какие ядра включить (для проверки по одному). Привод: AMPLITUDE (мкН на
мышцу), FREQUENCY (Гц), WAVELENGTH (доля L), DIRECTION
(+1 — волна от головы к хвосту), RAMP — время плавного нарастания, с,
PHASE — сдвиг фазы (рад), ANTAGONIST — чистый изгибающий момент."
  (let* ((set (if (worm.params:param-set-p params) params (worm.params:load-param-set params)))
         (tube (worm.topology:make-tube :rings rings :ring-nodes ring-nodes
                                        :length (param set :body-length)
                                        :radius-max (param set :radius-max)
                                        :radius-end-min (param set :radius-end-min)))
         (anatomy (worm.anatomy:load-anatomy))
         (binding (worm.anatomy:bind-muscles (worm.anatomy:anatomy-muscles anatomy)
                                             (worm.topology:tube-ring-fractions tube)
                                             (lambda (q i) (worm.topology:quadrant-long-edges tube q i))))
         (contact-p (and (member :contact kernels) (worm.params:param-value set :contact)))
         (medium (if (worm.params:param-value set :contact) :agar :water))
         (ei (param set :bending-ei))
         (soft (param set :tail-softening))
         (ratios (list :circ (param set :k-circ-ratio) :shear (param set :k-shear-ratio)
                       :diam (param set :k-diam-ratio)))
         (tau (param set :tau-cuticle))
         (stiffness (loop for f in worm.topology:+families+
                          collect f collect (edge-stiffness tube f ei soft ratios)))
         (viscosity (loop for (f k) on stiffness by #'cddr
                          collect f collect (map 'dvec (lambda (x) (* tau x)) k)))
         (n-nodes (worm.topology:tube-node-count tube))
         (n-long (worm.topology:family-size tube :long-edge))
         (n-comp (worm.topology:family-size tube :compartment))
         (kl (1- (* 6 ring-nodes)))
         (rest-volume (worm.topology:tube-rest-volume tube))
         (ring-ds (worm.topology:tube-ring-ds tube))
         (p (make-dvec (length +p-layout+)))
         ;; рабочие массивы замыканий
         (tension (make-dvec n-long))
         (centers (make-dvec (* 3 rings)))
         (tangents (make-dvec (* 3 rings)))
         (scratch (make-pressure-scratch ring-nodes))
         (check-scratch (make-pressure-scratch ring-nodes))
         (muscles (worm.anatomy:muscle-binding-muscles binding))
         (offsets (worm.anatomy:muscle-binding-offsets binding))
         (edges (worm.anatomy:muscle-binding-edges binding))
         (weights (worm.anatomy:muscle-binding-weights binding))
         (n-muscles (length muscles))
         (peaks (map 'dvec #'worm.anatomy:muscle-peak muscles))
         (signs (map 'dvec (lambda (m) (if (member (worm.anatomy:muscle-quadrant m)
                                                    '(:dorsal-left :dorsal-right))
                                           1d0 -1d0))
                     muscles))
         (families (remove-if-not (lambda (f) (member f kernels)) worm.topology:+families+)))
    (loop for (name value) in `((:c-t ,(param set :c-t)) (:c-n ,(param set :c-n))
                                (:bulk-modulus ,(param set :bulk-modulus))
                                (:meniscus-force ,(if (worm.params:param-entry-of set :meniscus-force)
                                                      (param set :meniscus-force) 0d0))
                                (:contact-stiffness ,(if (worm.params:param-entry-of set :contact-stiffness)
                                                         (param set :contact-stiffness) 0d0))
                                (:amplitude ,amplitude) (:frequency ,frequency)
                                (:wavelength ,wavelength) (:direction ,direction) (:ramp ,ramp)
                                (:phase ,phase) (:antagonist ,(if antagonist 1d0 0d0)))
          do (set-body-param p name value))
    (labels ((family-arrays (f)
               (multiple-value-bind (a b) (worm.topology:family-ends tube f)
                 (values a b (worm.topology:family-rest tube f) (getf stiffness f) (getf viscosity f))))
             (fill-tension (syn)
               (declare (type dvec syn))
               (fill tension 0d0)
               (when (member :tension kernels)
                 (dotimes (m n-muscles)
                   (loop for q from (aref offsets m) below (aref offsets (1+ m))
                         do (incf (aref tension (aref edges q)) (* (aref weights q) (aref syn m))))))))
      (let* ((family-data (mapcar (lambda (f) (multiple-value-list (family-arrays f))) families))
             (model
               (make-compiled-model
                :name 'oracle-body :n-mech (* 3 n-nodes) :lower-band kl :upper-band kl
                :n-syn n-muscles :n-observables rings
                :topology tube :anatomy anatomy
                :observables
                (lambda (y p obs)
                  (declare (ignore p))
                  (ring-curvatures y rings ring-nodes centers obs))
                :neural-output
                (lambda (yn z p clock syn)
                  (declare (ignore yn z) (type dvec p clock syn))
                  (let* ((time (+ (aref clock +clock-time+) (aref clock +clock-h+)))
                         (amp (aref p 5)) (f (aref p 6)) (lam (aref p 7)) (dir (aref p 8))
                         (ramp (aref p 9)) (phase (aref p 10))
                         (antagonist (plusp (aref p 11)))
                         (scale (if (plusp ramp) (min 1d0 (/ time ramp)) 1d0)))
                    (declare (type double-float time amp f lam dir ramp scale phase))
                    (unless (zerop amp)
                      (dotimes (m n-muscles)
                        (let ((v (* (aref signs m)
                                    (sin (+ phase (* 2d0 pi (- (* f time) (/ (* dir (aref peaks m)) lam))))))))
                          (declare (type double-float v))
                          (when (or antagonist (plusp v))
                            (incf (aref syn m) (* amp scale v)))))))
                  (values))
                :mech-forces
                (lambda (y syn p g)
                  (declare (type dvec y syn p g))
                  (fill-tension syn)
                  (loop for f in families for (a b rest k) in family-data
                        do (edge-forces y a b rest k (if (eq f :long-edge) tension nil) g))
                  (when (member :pressure kernels)
                    (pressure-forces y n-comp ring-nodes rest-volume (aref p 2) scratch g))
                  (when contact-p
                    (contact-forces y ring-ds rings ring-nodes (aref p 4) (aref p 3) g))
                  (values))
                :mech-matrix
                (lambda (y syn p clock ab)
                  (declare (type dvec y syn p clock ab))
                  (let ((h (aref clock +clock-h+)))
                    (fill-tension syn)
                    (loop for f in families for (a b rest k eta) in family-data
                          do (edge-matrix y a b rest k eta (if (eq f :long-edge) tension nil) h ab kl kl))
                    (when (member :pressure kernels)
                      (pressure-matrix y n-comp ring-nodes rest-volume (aref p 2) scratch h ab kl kl))
                    (when contact-p
                      (contact-matrix y n-nodes (aref p 4) h ab kl kl))
                    (when (member :drag kernels)
                      (ring-centers y rings ring-nodes centers)
                      (ring-tangents centers rings tangents)
                      (drag-matrix tangents ring-ds rings ring-nodes (aref p 0) (aref p 1) ab kl kl)))
                  (values))
                :check-state
                (lambda (y p)
                  (declare (ignore p) (type dvec y))
                  (dotimes (c n-comp nil)
                    (when (< (/ (compartment-geometry y c ring-nodes check-scratch nil)
                                (aref rest-volume c))
                             0.5d0)
                      (return (values :volume-collapse c))))))))
        (let* ((straight (worm.topology:tube-rest-coords tube))
               (shaped (if initial-curvature (arc-coords tube initial-curvature) (copy-seq straight)))
               (y0 (if contact-p (place-on-surface shaped tube) shaped)))
          (make-body :model model :tube tube :anatomy anatomy :binding binding :params set
                     :pvec p :y0 y0 :medium medium :kernels kernels
                     :long-stiffness (getf stiffness :long-edge) :rest-volume rest-volume))))))

(defun make-body-integrator (body &key (h 1d-4) (checkpoint-every 1000))
  (worm.solver:make-integrator (body-model body) :params (body-pvec body)
                               :y-mech (body-y0 body) :h h :neural-every 1
                               :checkpoint-every checkpoint-every))

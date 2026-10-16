;;;; tests/oracle/body.lisp — модель тела и среды из ручных ядер.
;;;;
;;;; Жёсткости рёбер выводятся из изгибной жёсткости EI(s) (параметры,
;;;; «Кутикула и ткани»): EI постоянна от головы до 2/3 длины и линейно
;;;; падает к хвосту до 1 − tail-softening (Backholm et al. 2013), продольное
;;;; ребро k_c(s) = 2·EI(s)/(K·R(s)²·Δs) — к тонким концам рёбра жёстче;
;;;; окружные рёбра, диагонали и диаметры — доли k_c; вязкость η = τ_c·k.
;;;; Привод — один из двух вариантов мышцы (дизайн, «Варианты мышцы»):
;;;;
;;;; - кинематический (этап 3): натяжение 95 клеток задано бегущей волной и
;;;;   рассеивается на продольные рёбра по привязке этапа 1; режим
;;;;   antagonist = 1 не обрезает отрицательную часть волны, тогда к сечению
;;;;   приложен чистый изгибающий момент без продольной силы;
;;;; - усреднённый (этап 5, :muscles t): генератор ритма уровня 1 даёт вход
;;;;   I клеток, кальций — в y-neural, натяжение F_max·a·f_L·f_V
;;;;   вычисляется перед механическим шагом (muscle.lisp, решение 22).
;;;;
;;;; Предварительное давление p₀ (гидростатика): давление отсека
;;;; p₀ + K·(V⁰ − V)/V⁰, а окружные и продольные рёбра несут в покое
;;;; натяжения, которые его уравновешивают (PRESTRESS-TENSIONS).
;;;;
;;;; Скалярные параметры, которые можно менять на ходу, лежат в векторе p
;;;; (+P-LAYOUT+); поэлементные величины — в замыканиях модели.

(in-package #:worm.oracle)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +p-layout+
    '(:c-t :c-n :bulk-modulus :meniscus-force :contact-stiffness
      :amplitude :frequency :wavelength :direction :ramp :phase :antagonist
      :prestress :tau-ca :ca-half :hill-n :syn-half :syn-width :hill-a-ratio
      :v-max :hill-eccentric :rate-filter)
    "Порядок скалярных параметров в векторе p ручной модели."))

(defmacro pref (p name)
  "Параметр NAME вектора p; номер известен при компиляции."
  `(aref ,p ,(or (position name +p-layout+) (error "нет параметра ~S в ручной модели" name))))

(defparameter +all-kernels+
  '(:long-edge :circ-edge :diagonal :diametral :pressure :drag :contact :tension))

(defun muscle-tensions (yn obs rings f-max lambda-min p syn)
  "Натяжение каждой клетки: F_max·a(Ca)·f_L(λ)·f_V(λ̄̇). YN — кальций и
сглаженные скорости, OBS — наблюдаемые (кривизна колец, затем λ), P — вектор
параметров: числа не передаются отдельно, чтобы не упаковывались."
  (declare (type dvec yn obs f-max lambda-min p syn) (type fixnum rings)
           (optimize (speed 3) (safety 1)))
  (let ((count (length f-max))
        (k (pref p :ca-half)) (n (pref p :hill-n)) (v-max (pref p :v-max))
        (a (pref p :hill-a-ratio)) (eccentric (pref p :hill-eccentric)))
    (dotimes (m count)
      (incf (aref syn m)
            (* (aref f-max m) (hill-activation (aref yn m) k n)
               (force-length (aref obs (+ rings m)) (aref lambda-min m))
               (force-velocity (aref yn (+ count m)) v-max a eccentric)))))
  (values))

(defstruct (body (:copier nil))
  model tube anatomy binding params pvec y0 medium kernels
  long-stiffness rest-volume
  muscles-p        ; T — усреднённые мышцы, NIL — кинематическое натяжение
  y0-neural        ; начальный кальций (стационарный при нулевом входе)
  f-max lambda-min ; поклеточные F_max и λ_min
  activations)     ; (lambda (integrator out)) — активации 0…1 для кадра

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

;;; Предварительное давление

(defun prestress-tensions (tube)
  "Натяжения рёбер в покое при единичном p₀: значения (окружные по кольцам,
продольные по промежуткам, диагональные по рёбрам, невязка). Прямое тело
покоя с давлением p₀ и натяжениями p₀·T в равновесии. Для узла кольца i
осевую составляющую силы давления p₀·∂V/∂x гасят продольные рёбра (по
цепочке от головы), радиальную — окружные, касательную — диагонали: две
закрутки промежутка несут +D и −D, их осевые и радиальные силы взаимно
сокращаются. Касательная сила возникает только на конических концах из-за
несимметричной триангуляции и закручивала бы их. Невязка — наибольшая
оставшаяся сила на узел в долях наибольшей силы давления."
  (let* ((kk (worm.topology:tube-ring-nodes tube))
         (rings (worm.topology:tube-rings tube))
         (y (worm.topology:tube-rest-coords tube))
         (g (make-dvec (length y)))
         (scratch (make-pressure-scratch kk))
         (circ (make-dvec rings))
         (long (make-dvec (1- rings)))
         (twist (make-dvec (1- rings)))
         (centers (make-dvec (* 3 rings))))
    (dotimes (c (1- rings))
      (compartment-geometry y c kk scratch nil)
      (let ((grad (pressure-scratch-grad scratch)) (base (* 3 c kk)))
        (dotimes (q (* 6 kk)) (incf (aref g (+ base q)) (aref grad q)))))
    (ring-centers y rings kk centers)
    (labels ((node (i k) (let ((p (* 3 (+ (* i kk) k))))
                           (list (aref y p) (aref y (+ p 1)) (aref y (+ p 2)))))
             (center (i) (list (aref centers (* 3 i)) (aref centers (+ (* 3 i) 1)) (aref centers (+ (* 3 i) 2))))
             (unit (v) (let ((n (sqrt (reduce #'+ (mapcar #'sq v))))) (mapcar (lambda (x) (/ x n)) v)))
             (dot (a b) (reduce #'+ (mapcar #'* a b)))
             (radial (i k) (unit (mapcar #'- (node i k) (center i))))
             (around (i k) (let ((r (radial i k))) (list 0d0 (- (third r)) (second r))))  ; e_x × e_r
             (force (i) (let ((p (* 3 (* i kk)))) (list (aref g p) (aref g (+ p 1)) (aref g (+ p 2))))))
      (let* ((ex '(1d0 0d0 0d0)) (sin-k (sin (/ pi kk)))
            (prev-t 0d0) (prev-u ex) (prev-d 0d0) (prev-dt-hi 0d0))
        (dotimes (i rings)
          (let* ((f (force i)) (er (radial i 0)) (et (around i 0))
                 (last (= i (1- rings)))
                 (u (if last ex (unit (mapcar #'- (node (1+ i) 0) (node i 0)))))
                 ;; касательная доля диагонали промежутка у нижнего конца и у верхнего:
                 ;; на конусе они различны
                 (dt (if last 0d0 (abs (dot (unit (mapcar #'- (node (1+ i) 1) (node i 0))) et))))
                 (dt-hi (if last 0d0 (abs (dot (unit (mapcar #'- (node i 0) (node (1+ i) 1)))
                                                (around (1+ i) 1)))))
                 (ti (if last 0d0 (/ (- (* prev-t (dot prev-u ex)) (dot f ex)) (dot u ex))))
                 (di (if last 0d0 (/ (- (* 2 prev-dt-hi prev-d) (dot f et)) (* 2 dt)))))
            (unless last (setf (aref long i) ti (aref twist i) di))
            (setf (aref circ i) (/ (+ (dot f er) (* ti (dot u er)) (- (* prev-t (dot prev-u er))))
                                   (* 2 sin-k)))
            (setf prev-t ti prev-u u prev-d di prev-dt-hi dt-hi))))
      ;; натяжение каждой диагонали: ±D по закрутке
      (multiple-value-bind (a b) (worm.topology:family-ends tube :diagonal)
        (let ((diag (make-dvec (length a))))
          (dotimes (e (length a))
            (let* ((na (aref a e)) (nb (aref b e))
                   (lo (min na nb)) (hi (max na nb))
                   (i (floor lo kk)) (k (mod lo kk))
                   (d (mapcar #'- (node (floor hi kk) (mod hi kk)) (node i k)))
                   (w (if (plusp (dot d (around i k))) 1d0 -1d0)))
              (setf (aref diag e) (* w (aref twist i)))))
          ;; невязка: силы давления и натяжений на всех узлах
          (let ((check (copy-seq g)))
            (dolist (fam '(:circ-edge :long-edge :diagonal))
              (multiple-value-bind (fa fb) (worm.topology:family-ends tube fam)
                (let ((tension (make-dvec (length fa))))
                  (dotimes (e (length fa))
                    (setf (aref tension e) (ecase fam
                                             (:circ-edge (aref circ (floor e kk)))
                                             (:long-edge (aref long (floor e kk)))
                                             (:diagonal (aref diag e)))))
                  (edge-forces y fa fb (worm.topology:family-rest tube fam) (make-dvec (length fa))
                               tension check))))
            (flet ((largest (v) (loop for i below (length v) by 3
                                      maximize (sqrt (+ (sq (aref v i)) (sq (aref v (+ i 1))) (sq (aref v (+ i 2))))))))
              (values circ long diag (/ (largest check) (largest g))))))))))

;;; Модель

(defun param (set name) (float (worm.params:param-value set name) 0d0))

(defun optional-param (set name default)
  (if (worm.params:param-entry-of set name) (param set name) default))

(defun build-body (&key (params "water-v1") (rings 50) (ring-nodes 8)
                        (kernels +all-kernels+) muscles
                        (amplitude (if muscles nil 0d0)) (frequency (if muscles nil 1d0))
                        (wavelength (if muscles nil 1d0)) (direction 1d0) (ramp 0d0) (phase 0d0)
                        antagonist initial-curvature prestress ei-scale)
  "Собрать ручную модель тела. PARAMS — имя набора параметров; KERNELS —
какие ядра включить (для проверки по одному).

MUSCLES = NIL — кинематическое натяжение: AMPLITUDE (мкН на мышцу),
FREQUENCY (Гц), WAVELENGTH (доля L), DIRECTION (+1 — волна от головы к
хвосту), RAMP — время плавного нарастания, с, PHASE — сдвиг фазы (рад),
ANTAGONIST — чистый изгибающий момент.

MUSCLES = T — усреднённые мышцы под генератором ритма уровня 1; AMPLITUDE —
нормированный вход A, по умолчанию gen-amplitude набора, FREQUENCY и
WAVELENGTH — по умолчанию gen-frequency и gen-wavelength набора.

PRESTRESS — p₀ в мкН/мм² вместо значения набора. EI-SCALE — добавочный
множитель жёсткости рёбер (для калибровки ei-correction)."
  (let* ((set (if (worm.params:param-set-p params) params (worm.params:load-param-set params)))
         (amplitude (or amplitude (param set :gen-amplitude)))
         (frequency (or frequency (param set :gen-frequency)))
         (wavelength (or wavelength (param set :gen-wavelength)))
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
         (ei (* (param set :bending-ei) (optional-param set :ei-correction 1d0) (or ei-scale 1d0)))
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
         (n-circ (worm.topology:family-size tube :circ-edge))
         (n-comp (worm.topology:family-size tube :compartment))
         (kl (1- (* 6 ring-nodes)))
         (rest-volume (worm.topology:tube-rest-volume tube))
         (ring-ds (worm.topology:tube-ring-ds tube))
         (p (make-dvec (length +p-layout+)))
         ;; рабочие массивы замыканий
         (tension (make-dvec n-long))
         (circ-tension (make-dvec n-circ))
         (centers (make-dvec (* 3 rings)))
         (tangents (make-dvec (* 3 rings)))
         (scratch (make-pressure-scratch ring-nodes))
         (check-scratch (make-pressure-scratch ring-nodes))
         (muscle-list (worm.anatomy:muscle-binding-muscles binding))
         (offsets (worm.anatomy:muscle-binding-offsets binding))
         (edges (worm.anatomy:muscle-binding-edges binding))
         (weights (worm.anatomy:muscle-binding-weights binding))
         (n-muscles (length muscle-list))
         (peaks (map 'dvec #'worm.anatomy:muscle-peak muscle-list))
         (signs (map 'dvec (lambda (m) (if (member (worm.anatomy:muscle-quadrant m)
                                                    '(:dorsal-left :dorsal-right))
                                           1d0 -1d0))
                     muscle-list))
         (families (remove-if-not (lambda (f) (member f kernels)) worm.topology:+families+))
         ;; предварительное давление: натяжения на единичное p₀
         (rest-long (make-dvec n-long))
         (rest-circ (make-dvec n-circ))
         (n-diag (worm.topology:family-size tube :diagonal))
         (rest-diag (make-dvec n-diag))
         (diag-tension (make-dvec n-diag))
         ;; мышцы
         (f-max (when muscles (muscle-f-max muscle-list set)))
         (lambda-min (when muscles (muscle-lambda-min muscle-list tube (param set :max-shortening))))
         (rest-sum (muscle-rest-lengths binding (worm.topology:family-rest tube :long-edge)))
         (lam (make-dvec n-muscles))
         (rate (make-dvec n-muscles)))
    (multiple-value-bind (ea eb) (worm.topology:family-ends tube :long-edge)
      (progn
        (multiple-value-bind (circ long diag) (prestress-tensions tube)
          (dotimes (e n-long) (setf (aref rest-long e) (aref long (floor e ring-nodes))))
          (dotimes (e n-circ) (setf (aref rest-circ e) (aref circ (floor e ring-nodes))))
          (replace rest-diag diag)))
      (loop for (name value) in `((:c-t ,(param set :c-t)) (:c-n ,(param set :c-n))
                                  (:bulk-modulus ,(param set :bulk-modulus))
                                  (:meniscus-force ,(optional-param set :meniscus-force 0d0))
                                  (:contact-stiffness ,(optional-param set :contact-stiffness 0d0))
                                  (:amplitude ,amplitude) (:frequency ,frequency)
                                  (:wavelength ,wavelength) (:direction ,direction) (:ramp ,ramp)
                                  (:phase ,phase) (:antagonist ,(if antagonist 1d0 0d0))
                                  (:prestress ,(or prestress (param set :prestress))))
            do (set-body-param p name value))
      (when muscles
        (dolist (name '(:tau-ca :ca-half :hill-n :syn-half :syn-width :hill-a-ratio :v-max :hill-eccentric
                        :rate-filter))
          (set-body-param p name (param set name))))
      (labels ((family-arrays (f)
                 (multiple-value-bind (a b) (worm.topology:family-ends tube f)
                   (values a b (worm.topology:family-rest tube f) (getf stiffness f) (getf viscosity f))))
               (fill-tension (syn p)
                 (declare (type dvec syn p))
                 (let ((p0 (pref p :prestress)))
                   (scale-into tension rest-long p0)
                   (scale-into circ-tension rest-circ p0)
                   (scale-into diag-tension rest-diag p0))
                 (when (member :tension kernels)
                   (scatter-muscle-tension tension offsets edges weights syn)))
               (family-tension (f)
                 (case f (:long-edge tension) (:circ-edge circ-tension) (:diagonal diag-tension) (t nil)))
               (drive (p time m)
                 ;; вход генератора ритма клетки M в момент TIME (кинематический — натяжение)
                 (let* ((amp (pref p :amplitude)) (ramp (pref p :ramp))
                        (scale (if (plusp ramp) (min 1d0 (/ time ramp)) 1d0))
                        (v (* (aref signs m)
                              (sin (+ (pref p :phase)
                                      (* 2d0 pi (- (* (pref p :frequency) time)
                                                   (/ (* (pref p :direction) (aref peaks m))
                                                      (pref p :wavelength)))))))))
                   (if (or (plusp v) (and (not muscles) (plusp (pref p :antagonist))))
                       (* amp scale v)
                       0d0))))
        (let* ((family-data (mapcar (lambda (f) (multiple-value-list (family-arrays f))) families))
               (n-obs (if muscles (+ rings (* 2 n-muscles)) rings))
               (model
                 (make-compiled-model
                  :name (if muscles 'oracle-muscles 'oracle-body)
                  :n-mech (* 3 n-nodes) :lower-band kl :upper-band kl
                  :n-neural (if muscles (* 2 n-muscles) 0) :n-aux (if muscles (* 3 n-nodes) 0)
                  :n-syn n-muscles :n-observables n-obs
                  :topology tube :anatomy anatomy
                  :observables
                  (lambda (y aux p obs)
                    (declare (ignore p) (type dvec y aux obs))
                    (ring-curvatures y rings ring-nodes centers obs)
                    (when muscles
                      (muscle-geometry y aux offsets edges weights ea eb rest-sum lam rate)
                      (dotimes (m n-muscles)
                        (incf (aref obs (+ rings m)) (aref lam m))
                        (incf (aref obs (+ rings n-muscles m)) (aref rate m))))
                    (values))
                  :neural-rhs
                  (when muscles
                    ;; y-neural: кальций клеток, затем сглаженные скорости их длины
                    (lambda (yn obs z p clock f)
                      (declare (ignore z) (type dvec yn obs p clock f))
                      (let ((time (aref clock +clock-time+))
                            (tau (pref p :tau-ca)) (tau-v (pref p :rate-filter))
                            (half (pref p :syn-half)) (width (pref p :syn-width)))
                        (dotimes (m n-muscles)
                          (incf (aref f m) (/ (- (ca-steady (drive p time m) half width) (aref yn m)) tau))
                          (incf (aref f (+ n-muscles m))
                                (/ (- (aref obs (+ rings n-muscles m)) (aref yn (+ n-muscles m))) tau-v))))
                      (values)))
                  :neural-jacobian
                  (when muscles
                    (lambda (yn obs z p clock jac)
                      (declare (ignore yn obs z clock) (type dvec p) (type dmat jac))
                      (let ((d (/ -1d0 (pref p :tau-ca))) (dv (/ -1d0 (pref p :rate-filter))))
                        (dotimes (m n-muscles)
                          (incf (aref jac m m) d)
                          (incf (aref jac (+ n-muscles m) (+ n-muscles m)) dv)))
                      (values)))
                  :neural-output
                  (lambda (yn obs z p clock syn)
                    (declare (ignore z) (type dvec yn obs p clock syn))
                    (if muscles
                        (muscle-tensions yn obs rings f-max lambda-min p syn)
                        (let ((time (+ (aref clock +clock-time+) (aref clock +clock-h+))))
                          (unless (zerop (pref p :amplitude))
                            (dotimes (m n-muscles)
                              (incf (aref syn m) (drive p time m))))))
                    (values))
                  :mech-forces
                  (lambda (y syn p g)
                    (declare (type dvec y syn p g))
                    (fill-tension syn p)
                    (loop for f in families for (a b rest k) in family-data
                          do (edge-forces y a b rest k (family-tension f) g))
                    (when (member :pressure kernels)
                      (pressure-forces y n-comp ring-nodes rest-volume (pref p :bulk-modulus)
                                       (pref p :prestress) scratch g))
                    (when contact-p
                      (contact-forces y ring-ds rings ring-nodes (pref p :contact-stiffness)
                                      (pref p :meniscus-force) g))
                    (values))
                  :mech-matrix
                  (lambda (y aux syn p clock ab)
                    (declare (type dvec y aux syn p clock ab) (ignore aux))
                    (let ((h (aref clock +clock-h+)))
                      (fill-tension syn p)
                      (loop for f in families for (a b rest k eta) in family-data
                            do (edge-matrix y a b rest k eta (family-tension f) clock ab kl kl))
                      (when (member :pressure kernels)
                        (pressure-matrix y n-comp ring-nodes rest-volume (pref p :bulk-modulus)
                                         (pref p :prestress) scratch h ab kl kl))
                      (when contact-p
                        (contact-matrix y n-nodes (pref p :contact-stiffness) h ab kl kl))
                      (when (member :drag kernels)
                        (ring-centers y rings ring-nodes centers)
                        (ring-tangents centers rings tangents)
                        (drag-matrix tangents ring-ds rings ring-nodes (pref p :c-t) (pref p :c-n) ab kl kl)))
                    (values))
                  :check-state
                  (lambda (y p)
                    (declare (ignore p) (type dvec y))
                    (dotimes (c n-comp nil)
                      (%compartment-geometry y c ring-nodes check-scratch nil)
                      (when (< (/ (aref (pressure-scratch-volume check-scratch) 0) (aref rest-volume c))
                               0.5d0)
                        (return (values :volume-collapse c))))))))
          (let* ((straight (worm.topology:tube-rest-coords tube))
                 (shaped (if initial-curvature (arc-coords tube initial-curvature) (copy-seq straight)))
                 (y0 (if contact-p (place-on-surface shaped tube) shaped))
                 (ca0 (when muscles
                        (let ((v (make-dvec (* 2 n-muscles)))
                              (c (ca-steady 0d0 (pref p :syn-half) (pref p :syn-width))))
                          (dotimes (m n-muscles v) (setf (aref v m) c))))))
            (make-body :model model :tube tube :anatomy anatomy :binding binding :params set
                       :pvec p :y0 y0 :medium medium :kernels kernels
                       :long-stiffness (getf stiffness :long-edge) :rest-volume rest-volume
                       :muscles-p (and muscles t) :y0-neural ca0 :f-max f-max :lambda-min lambda-min
                       :activations
                       (if muscles
                           (lambda (it out)
                             (let ((ca (worm.solver:integrator-y-neural it))
                                   (pv (worm.solver:integrator-params it)))
                               (dotimes (m n-muscles)
                                 (setf (aref out m)
                                       (float (hill-activation (aref ca m) (pref pv :ca-half) (pref pv :hill-n))
                                              0f0)))))
                           (lambda (it out)
                             (let ((syn (worm.solver:integrator-syn it))
                                   (amp (pref (worm.solver:integrator-params it) :amplitude)))
                               (dotimes (m n-muscles)
                                 (setf (aref out m)
                                       (if (plusp amp)
                                           (float (max 0d0 (min 1d0 (/ (abs (aref syn m)) amp))) 0f0)
                                           0f0)))))))))))))

(defun make-body-integrator (body &key (h 1d-4) (checkpoint-every 1000) (neural-every 10))
  (worm.solver:make-integrator (body-model body) :params (body-pvec body)
                               :y-mech (body-y0 body) :y-neural (body-y0-neural body)
                               :h h :neural-every neural-every
                               :checkpoint-every checkpoint-every))

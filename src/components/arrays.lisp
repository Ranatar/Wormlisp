;;;; src/components/arrays.lisp — поэлементные массивы модели.
;;;;
;;;; Строятся из скалярных параметров и топологии при сборке модели
;;;; (параметры, «Кутикула и ткани», «Гидростатика», «Мышцы»):
;;;;
;;;;   k-…    жёсткость рёбер: k_c(s) = 2·EI·c·хвост(s)/(K·R(s)²·Δs), где
;;;;          c — ei-correction, хвост — 1 до 2/3 длины и линейно до
;;;;          1 − tail-softening к хвосту; окружные, диагонали и диаметры —
;;;;          доли k_c (k-circ-ratio, k-shear-ratio, k-diam-ratio);
;;;;   eta-…  вязкость рёбер τ_c·k;
;;;;   t-…    натяжение покоя на единичное p₀ (worm.topology:prestress-tensions);
;;;;   muscle-f-max, muscle-lambda-min — сила и предельное укорочение клеток.

(in-package #:worm.components)

(defun param (context name) (float (worm.params:param-value (getf context :params) name) 0d0))

(defun tail-factor (s softening)
  (- 1d0 (* softening (max 0d0 (min 1d0 (/ (- s 2/3) 1/3))))))

(defun edge-stiffness (context family)
  "Жёсткость рёбер семейства FAMILY."
  (let* ((tube (getf context :topology))
         (kk (worm.topology:tube-ring-nodes tube))
         (rings (worm.topology:tube-rings tube))
         (radius (worm.topology:tube-ring-radius tube))
         (frac (worm.topology:tube-ring-fractions tube))
         (ds (/ (worm.topology:tube-length tube) (1- rings)))
         (ei (* (param context :bending-ei) (param context :ei-correction)))
         (soft (param context :tail-softening))
         (out (make-dvec (worm.topology:family-size tube family))))
    (flet ((kc (r s) (/ (* 2 ei (tail-factor s soft)) (* kk r r ds))))
      (ecase family
        ((:long-edge :diagonal)
         (let ((per-ring (if (eq family :long-edge) kk (* 2 kk)))
               (ratio (if (eq family :long-edge) 1d0 (param context :k-shear-ratio))))
           (dotimes (i (1- rings))
             (let ((k (* ratio (kc (* 0.5d0 (+ (aref radius i) (aref radius (1+ i))))
                                   (* 0.5d0 (+ (aref frac i) (aref frac (1+ i))))))))
               (dotimes (j per-ring) (setf (aref out (+ (* i per-ring) j)) k))))))
        ((:circ-edge :diametral)
         (let ((per-ring (if (eq family :circ-edge) kk (/ kk 2)))
               (ratio (param context (if (eq family :circ-edge) :k-circ-ratio :k-diam-ratio))))
           (dotimes (i rings)
             (let ((k (* ratio (kc (aref radius i) (aref frac i)))))
               (dotimes (j per-ring) (setf (aref out (+ (* i per-ring) j)) k))))))))
    out))

(defun edge-viscosity (context family)
  (let ((tau (param context :tau-cuticle)))
    (map 'dvec (lambda (k) (* tau k)) (edge-stiffness context family))))

(defun unit-prestress (context family)
  "Натяжение покоя каждого ребра на единичное p₀."
  (let* ((tube (getf context :topology))
         (kk (worm.topology:tube-ring-nodes tube))
         (n (worm.topology:family-size tube family))
         (out (make-dvec n)))
    (unless (eq family :diametral)
      (multiple-value-bind (circ long diag) (worm.topology:prestress-tensions tube)
        (dotimes (e n)
          (setf (aref out e) (ecase family
                               (:long-edge (aref long (floor e kk)))
                               (:circ-edge (aref circ (floor e kk)))
                               (:diagonal (aref diag e)))))))
    out))

(macrolet ((edge-arrays (&rest specs)
             `(progn
                ,@(loop for (family k eta tension) in specs
                        append `((define-element-array ,k :family ,family
                                   :compute (lambda (c) (edge-stiffness c ,family))
                                   :documentation "Жёсткость рёбер, мкН/мм.")
                                 (define-element-array ,eta :family ,family
                                   :compute (lambda (c) (edge-viscosity c ,family))
                                   :documentation "Вязкость рёбер τ_c·k, мкН·с/мм.")
                                 (define-element-array ,tension :family ,family
                                   :compute (lambda (c) (unit-prestress c ,family))
                                   :documentation "Натяжение покоя на единичное p₀, мкН на мкН/мм²."))))))
  (edge-arrays (:long-edge k-long eta-long t-long)
               (:circ-edge k-circ eta-circ t-circ)
               (:diagonal k-diag eta-diag t-diag)
               (:diametral k-diam eta-diam t-diam)))

;;; Мышцы

(defun muscle-f-max (context)
  "F_max каждой клетки: F_max середины × линейный градиент от головы к хвосту
(нормирован на середину) × ослабление первой клетки каждого ряда."
  (let* ((muscles (worm.anatomy:muscle-binding-muscles (getf context :binding)))
         (set (getf context :params))
         (mid (param context :f-max-mid))
         (grad (worm.params:param-value set :f-max-gradient))
         (g0 (float (aref grad 0) 0d0)) (g1 (float (aref grad 1) 0d0))
         (weak (param context :head-weakening)))
    (map 'dvec (lambda (m)
                 (* mid (/ (+ g0 (* (- g1 g0) (worm.anatomy:muscle-peak m))) (* 0.5d0 (+ g0 g1)))
                    (if (<= (worm.anatomy:muscle-index m) 2) weak 1d0)))
         muscles)))

(defun muscle-lambda-min (context)
  "λ_min каждой клетки: 1 − Δ_M·r/R_max, r — радиус тела у её максимума."
  (let* ((muscles (worm.anatomy:muscle-binding-muscles (getf context :binding)))
         (tube (getf context :topology))
         (frac (worm.topology:tube-ring-fractions tube))
         (radius (worm.topology:tube-ring-radius tube))
         (r-max (worm.topology:tube-radius-max tube))
         (rings (length frac))
         (delta (param context :max-shortening)))
    (map 'dvec (lambda (m)
                 (let* ((s (worm.anatomy:muscle-peak m))
                        (i (min (- rings 2) (max 0 (floor (* s (1- rings))))))
                        (u (/ (- s (aref frac i)) (- (aref frac (1+ i)) (aref frac i))))
                        (r (+ (* (- 1d0 u) (aref radius i)) (* u (aref radius (1+ i))))))
                   (- 1d0 (/ (* delta r) r-max))))
         muscles)))

(define-element-array muscle-f-max :family :muscle :compute #'muscle-f-max
  :documentation "F_max клетки, мкН.")
(define-element-array muscle-lambda-min :family :muscle :compute #'muscle-lambda-min
  :documentation "Предельная относительная длина клетки.")

;;; Нейроны коннектома (уровень 3, этап 12)

(defun solve-dense (a b)
  "Решение A·x = B методом Гаусса с выбором ведущего элемента; A и B портятся."
  (let ((n (length b)))
    (dotimes (k n)
      (let ((p (loop with best = k for i from k below n
                     when (> (abs (aref a i k)) (abs (aref a best k))) do (setf best i)
                     finally (return best))))
        (when (/= p k)
          (dotimes (j n) (rotatef (aref a k j) (aref a p j)))
          (rotatef (aref b k) (aref b p)))
        (loop for i from (1+ k) below n
              do (let ((f (/ (aref a i k) (aref a k k))))
                   (unless (zerop f)
                     (loop for j from k below n do (decf (aref a i j) (* f (aref a k j))))
                     (decf (aref b i) (* f (aref b k))))))))
    (let ((x (make-dvec n)))
      (loop for i from (1- n) downto 0
            do (setf (aref x i) (/ (- (aref b i) (loop for j from (1+ i) below n sum (* (aref a i j) (aref x j))))
                                   (aref a i i))))
      x)))

(defun neuron-threshold (context)
  "Порог V_th каждого нейрона: в покое сети без внешних токов сигмоида
синапса каждого нейрона равна φ_r (neuron-rest-activity; у Kunert et al.
2014 φ_r = 1/2). Тогда s_eq = a_r·φ_r/(a_r·φ_r + a_d), потенциал покоя V_eq
решает линейную систему G_c(V − E_c) + g·Σ_j gap_ij(V_i − V_j) +
g·s_eq·Σ_j c_ij(V_i − E_j) = 0, а V_th = V_eq − ln(φ_r/(1 − φ_r))/β."
  (let* ((connectome (worm.anatomy:anatomy-connectome (getf context :anatomy)))
         (neurons (worm.anatomy:connectome-neurons connectome))
         (n (length neurons))
         (gc (param context :neuron-g-leak)) (ec (param context :neuron-e-leak))
         (g (param context :contact-g))
         (e-exc (param context :e-exc)) (e-inh (param context :e-inh))
         (ar (param context :syn-rise)) (ad (param context :syn-decay))
         (phi (param context :neuron-rest-activity))
         (beta (param context :syn-beta))
         (s-eq (/ (* ar phi) (+ (* ar phi) ad)))
         (a (make-array (list n n) :element-type 'double-float :initial-element 0d0))
         (b (make-dvec n)))
    (dotimes (i n)
      (incf (aref a i i) gc)
      (incf (aref b i) (* gc ec)))
    (loop for (i j c) in (worm.dsl::scaled-gaps connectome (getf context :params))
          do (incf (aref a i i) (* g c)) (incf (aref a j j) (* g c))
             (decf (aref a i j) (* g c)) (decf (aref a j i) (* g c)))
    (loop for (pre post c) in (worm.dsl::scaled-chemical connectome (getf context :params))
          for e = (if (eq (worm.anatomy:neuron-transmitter (aref neurons pre)) :gaba) e-inh e-exc)
          do (incf (aref a post post) (* g s-eq c))
             (incf (aref b post) (* g s-eq c e)))
    (let ((shift (/ (log (/ phi (- 1 phi))) beta)))
      (map 'dvec (lambda (v) (- v shift)) (solve-dense a b)))))

(define-element-array neuron-threshold :family :neuron :compute #'neuron-threshold
  :documentation "Порог синаптической сигмоиды нейрона, мВ: потенциал покоя сети при φ = 1/2.")

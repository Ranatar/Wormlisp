;;;; src/components/body.lisp — тело, среда, мышцы и генератор ритма на языке модели.
;;;;
;;;; Уравнения — по дизайну («Модель тела», «Среда», «Модель мышцы»,
;;;; «Нервное управление», уровень 1) и решению 22: кальций и сглаженная
;;;; скорость длины клетки — в нервно-мышечной подсистеме, её натяжение
;;;; вычисляется по длине и скорости начала шага и в механическом шаге
;;;; постоянно.

(in-package #:worm.components)

(define-component cuticle-edge
  :family :edge
  :params ((stiffness :per-element) (viscosity :per-element) (rest-tension :per-element) prestress)
  :inputs ((tension (link :muscle-tension) :default 0))
  :let ((u (unit edge-vector))
        (f (+ (* stiffness (- edge-length rest-length)) (* prestress rest-tension) tension)))
  :forces (((pos a) (scale f u))
           ((pos b) (scale (- f) u)))
  :damping ((viscosity length-rate))
  :outputs ((length edge-length)
            (rest rest-length)
            (rate (lagged length-rate)))
  :documentation "Ребро кутикулы: упругость, натяжение покоя от p₀, натяжение мышц, вязкость.
Выходы — длина, длина покоя и скорость удлинения прошлого шага — читают мышцы.")

(define-component hydrostatic
  :family :compartment
  :params (bulk-modulus prestress)
  :let ((p (+ prestress (* bulk-modulus (/ (- rest-volume volume) rest-volume)))))
  :forces ((:gradient volume :scale p))
  :checks ((:volume-collapse (- (/ volume rest-volume) 0.5)))
  :documentation "Давление отсека p₀ + K·(V⁰ − V)/V⁰; сила p·∂V/∂x; сбой при V/V⁰ < 0,5.")

(define-component substrate
  :family :node
  :params (contact-stiffness meniscus-force)
  :let ((z (dot (pos self) (vec 0 0 1))))
  :forces (((pos self) (vec 0 0 (- (* contact-stiffness (positive (- z))) (* meniscus-force ds)))))
  :documentation "Контакт с поверхностью z = 0: отталкивание k_s·max(0, −z), прижим мениска f_m·Δs/K.")

(define-component rft
  :family :node
  :params (c-t c-n)
  :damping (((* (- c-t c-n) ds) (dot tangent (velocity self)))
            ((* c-n ds) (dot (vec 1 0 0) (velocity self)))
            ((* c-n ds) (dot (vec 0 1 0) (velocity self)))
            ((* c-n ds) (dot (vec 0 0 1) (velocity self))))
  :documentation "Сопротивление среды по теории резистивных сил: (C_t·ttᵀ + C_n·(I − ttᵀ))·Δs/K.")

(define-component rhythm-generator
  :family :muscle
  :params (gen-amplitude gen-frequency gen-wavelength gen-direction gen-ramp)
  :outputs ((drive (* gen-amplitude (clamp (/ (time) gen-ramp) 0 1)
                      (positive (* side (sin (* 2 pi (- (* gen-frequency (time))
                                                         (/ (* gen-direction position) gen-wavelength)))))))))
  :documentation "Генератор ритма уровня 1: бегущая волна входа, дорсальная и вентральная стороны в противофазе.")

(define-component rate-muscle
  :family :muscle
  :params (tau-ca ca-half hill-n syn-half syn-width v-max hill-a-ratio hill-eccentric rate-filter
           (f-max :per-element) (lambda-min :per-element))
  :state ((ca (logistic (/ (- syn-half) syn-width)))
          (rate 0))
  :inputs ((drive (link :drive))
           (attached-length (link :attached-length))
           (attached-rest (link :attached-rest))
           (attached-rate (link :attached-rate)))
  :let ((lam (/ attached-length attached-rest))
        (lam-rate (/ attached-rate attached-rest))
        (u (/ (- rate) v-max))
        (shorten (positive u))
        (lengthen (positive (- u)))
        (c (/ (* (- hill-eccentric 1) hill-a-ratio) (+ 1 hill-a-ratio)))
        (f-l (positive (- 1 (expt (/ (- lam 1) (- 1 lambda-min)) 2))))
        (f-v (+ (positive (/ (- 1 shorten) (+ 1 (/ shorten hill-a-ratio))))
                (/ (* (- hill-eccentric 1) lengthen) (+ lengthen c))))
        (activation (hill ca ca-half hill-n)))
  :derivs ((ca (/ (- (logistic (/ (- drive syn-half) syn-width)) ca) tau-ca))
           (rate (/ (- lam-rate rate) rate-filter)))
  :outputs ((force (* f-max activation f-l f-v))
            (active activation))
  :documentation "Усреднённая мышца: τ_Ca·dCa/dt = Ca∞(I) − Ca, a = hill(Ca), натяжение F_max·a·f_L(λ)·f_V(λ̄̇);
τ_v·dλ̄̇/dt = λ̇ − λ̄̇. Длина λ = Σwℓ/Σwℓ₀ по прикреплённым рёбрам.")

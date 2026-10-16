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

;;; Уровень 2: проприоцептивная цепь (бинарная модель Бойла, Берри и Коэна 2012)

(define-component stretch-receptor
  :family :long-edge
  :outputs ((bend-strain (* 0.5 side radius-factor (- (/ edge-length rest-length) 1))))
  :documentation "Рецептор растяжения: изгибная часть относительного удлинения продольного ребра —
полуразность дорсальной и вентральной сторон после суммирования по полю, приведённая к толщине тела
множителем R_max/R. Общее укорочение тела при сокращении мышц в неё не входит (параметры, «Уровень 2»).")

(define-component b-unit
  :family :prop-unit
  :params (avb-dorsal avb-ventral sr-gain-base sr-gain-slope sr-scale dorsal-stretch-gain dorsal-compress-gain
           b-threshold hysteresis vd-inhibition sr-filter)
  :state ((sense 0))
  :discrete ((db 0) (vb 0))
  :inputs ((bend (link :stretch)))
  :derivs ((sense (/ (- bend sense) sr-filter)))
  :let ((g (* sr-scale (+ sr-gain-base (* sr-gain-slope index))))
        (sd (- (* dorsal-stretch-gain (positive sense))
               (* dorsal-compress-gain (positive (- sense)))))
        (i-db (+ avb-dorsal (* g sd)))
        (i-vb (- (- avb-ventral (* g sense)) (* vd-inhibition db)))
        (on (* b-threshold (+ 1 hysteresis)))
        (off (* b-threshold (- 1 hysteresis))))
  :events (((- i-db on) :rising (set db 1))
           ((- i-db off) :falling (set db 0))
           ((- i-vb on) :rising (set vb 1))
           ((- i-vb off) :falling (set vb 0)))
  :outputs ((motor (- db vb)))
  :documentation "Проприоцептивная единица: дорсальный и вентральный B-нейроны с порогом и гистерезисом.
Вход — AVB и растяжение своей стороны при изгибе в рецептивном поле (дорсальное — bend,
вентральное — −bend), сглаженное с постоянной τ_sr (передача сигнала рецептора); VD, возбуждаемый DB,
тормозит VB. Выход db − vb: мышцы своей стороны получают его со знаком стороны —
B возбуждает свою сторону, D (через B противоположной стороны) тормозит.")

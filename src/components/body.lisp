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
           b-threshold hysteresis vd-inhibition sr-filter head-sr-gain head-adapt-gain head-adapt-tau head-bias)
  :state ((sense 0) (adapt 0))
  :discrete ((db 0) (vb 0))
  :inputs ((bend (link :stretch)))
  :derivs ((sense (/ (- bend sense) sr-filter))
           (adapt (/ (- (- db vb) adapt) head-adapt-tau)))
  :let ((g (* sr-scale (+ sr-gain-base (* sr-gain-slope index))
                   ;; головная единица (index = 1): своё усиление
                   (+ 1 (* (- head-sr-gain 1) (positive (- 2 index))))))
        (sd (- (* dorsal-stretch-gain (positive sense))
               (* dorsal-compress-gain (positive (- sense)))))
        (head (positive (- 2 index)))
        ;; утомление головной единицы: медленный ток против включённой стороны
        (i-adapt (* head-adapt-gain head adapt))
        (i-db (- (+ avb-dorsal (* g sd)) i-adapt))
        (i-vb (+ (- (- avb-ventral (* g sense)) (* vd-inhibition db)) i-adapt (* head-bias head)))
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

(define-component b-unit-graded
  :family :prop-unit
  :params (sr-gain-base sr-gain-slope sr-scale dorsal-stretch-gain dorsal-compress-gain sr-filter
           gb-c gb-g-mem gb-i-act gb-i-gaba gb-g-avb gb-v-rest gb-v-act gb-v-avb gb-v-nmj
           gb-k-act gb-k-gaba gb-k-nmj gb-bias-ventral gb-w-ach gb-w-gaba gb-sr-current gb-nmj-scale)
  :state ((sense 0) (ud 0) (uv (* 2 (- gb-v-act gb-v-rest))))
  :inputs ((bend (link :stretch)))
  :let ((g (* gb-sr-current sr-scale (+ sr-gain-base (* sr-gain-slope index))))
        (sd (- (* dorsal-stretch-gain (positive sense))
               (* dorsal-compress-gain (positive (- sense)))))
        (du-act (- gb-v-act gb-v-rest))
        (du-avb (- gb-v-avb gb-v-rest))
        (du-nmj (- gb-v-nmj gb-v-rest))
        (i-d (+ (- (* gb-g-mem ud))
                (* gb-i-act (logistic (* gb-k-act (- ud du-act))))
                (* gb-g-avb (- du-avb ud))
                (* g sd)))
        (i-v (+ (- (* gb-g-mem uv))
                (* gb-i-act (logistic (* gb-k-act (- uv du-act))))
                (* gb-g-avb (- du-avb uv))
                gb-bias-ventral
                (- (* g sense))
                (- (* gb-i-gaba (logistic (* gb-k-gaba ud))))))
        (nmj-d (logistic (* gb-k-nmj (- ud du-nmj))))
        (nmj-v (logistic (* gb-k-nmj (- uv du-nmj)))))
  :derivs ((sense (/ (- bend sense) sr-filter))
           (ud (/ i-d gb-c))
           (uv (/ i-v gb-c)))
  :outputs ((motor (* gb-nmj-scale 0.5 (- gb-w-ach gb-w-gaba) (- nmj-d nmj-v))))
  :documentation "Проприоцептивная единица, проводимостная версия Бойла (2012, ур. 14–15): мембранные
потенциалы DB и VB, отсчитанные от покоя (u = V − V_rest), с утечкой, самовозбуждающим током
I_act·σ(k_act(V − V_act)) (бистабильность), щелевым контактом AVB, током смещения вентральной
стороны и торможением VB от VD, которое следует за DB: −I_GABA·σ(k_GABA(V_D − V_rest)). Ток рецептора —
тот же сглаженный сигнал изгиба, что у бинарной версии, в пА: I_sr·s·(a + b·n). Нервно-мышечный
синапс — сигмоида σ(k_NMJ(V − V₀,NMJ)) с весами w_ACh своей стороны и w_GABA противоположной;
выход — полуразность входов дорсальных и вентральных мышц, (w_ACh − w_GABA)/2·(σ_D − σ_V), с
множителем гибкости нервно-мышечного синапса (общая часть обеих сторон отброшена, как у бинарной версии).
VB начинает в верхнем состоянии, как в бинарной версии: в прямом теле сигнала рецептора нет, а VD
в покое тормозит VB вполсилы (V₀,GABA = V_rest), и без начального толчка обе стороны остаются в покое.")

;;; Уровень 3: коннектом (Kunert et al. 2014, этап 12)

(define-component graded-neuron
  :family :neuron
  :params (neuron-c neuron-g-leak neuron-e-leak contact-g e-exc e-inh syn-rise syn-decay syn-beta
           command-current neuron-sr-conductance neuron-nmj-gain nmj-inhibition-scale nmj-head-scale sr-filter
           head-drive head-drive-frequency
           (neuron-threshold :per-element))
  :state ((v neuron-e-leak) (s 0) (sense 0))
  :inputs ((gap-in (link :gap))
           (exc-in (link :exc))
           (inh-in (link :inh))
           (bend (link :stretch) :default 0))
  :let ((i-gap (* contact-g (- gap-in (* gap-total v))))
        (i-syn (- (+ (* contact-g exc-in (- v e-exc)) (* contact-g inh-in (- v e-inh)))))
        (i-leak (* neuron-g-leak (- neuron-e-leak v)))
        ;; растяжение своей стороны открывает возбуждающий канал, сжатие —
        ;; тормозящий: ток ограничен потенциалами реверсии
        (x (* proprioceptive side sense))
        (i-sr (* neuron-sr-conductance (+ (* (positive x) (- e-exc v)) (* (positive (- x)) (- e-inh v)))))
        (phi (logistic (* syn-beta (- v neuron-threshold))))
        (hd (* head-side (sin (* 2 pi head-drive-frequency (time)))))
        (i-head (* head-drive (+ (* (positive hd) (- e-exc v)) (* (positive (- hd)) (- e-inh v))))))
  :derivs ((v (/ (+ i-leak i-gap i-syn i-sr (* command command-current)
                    ;; проверочный головной ритм: канал с потенциалами реверсии
                    ;; в головных мотонейронах, как у рецептора растяжения
                    i-head)
                 neuron-c))
           (s (- (* syn-rise phi (- 1 s)) (* syn-decay s)))
           (sense (/ (- bend sense) sr-filter)))
  :outputs ((potential v)
            (s-exc (* s (- 1 gaba)))
            (s-inh (* s gaba))
            (motor (* neuron-nmj-gain (+ cord (* (- 1 cord) nmj-head-scale))
                      (/ s (/ syn-rise (+ syn-rise syn-decay)))
                      (- (- 1 gaba) (* nmj-inhibition-scale gaba)))))
  :documentation "Градуальный нейрон коннектома (Kunert et al. 2014): C·dV/dt = −G_c(V − E_c)
− g·Σ gap_ij(V_i − V_j) − g·Σ c_ij s_j(V_i − E_j) + I; синапс ds/dt = a_r·φ(V)(1 − s) − a_d·s,
φ = σ(β(V − V_th)), V_th — потенциал покоя сети при φ = 1/2. Возбуждающие и тормозные (ГАМК)
синапсы сводятся отдельно, потому что у них разные потенциалы реверсии. B-нейроны (DB, VB)
чувствуют растяжение своей стороны: изгиб поля назад от нейрона, сглаженный с τ_sr, как у второго
уровня; растяжение открывает канал с потенциалом реверсии E_exc, сжатие — с E_inh, проводимость
g_sr·|x|, поэтому потенциал остаётся между E_inh и E_exc; команда вперёд — ток +I в AVB и −I в AVA. Выход на мышцы — активность
синапса, отнесённая к наибольшей a_r/(a_r + a_d), со знаком медиатора, с усилением
нервно-мышечного синапса; у нейронов вне тяжа — ещё с множителем nmj-head-scale. Вход мышцы —
сумма с весами доли контактов, поэтому полностью активные мотонейроны тяжа дают вход, равный
усилению, как ±1 бинарной B-единицы второго уровня.")

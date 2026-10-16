;;;; tests/dsl.lisp — язык модели: define-component, defmodel, интерпретатор (этап 6).
;;;;
;;;; Условия готовности: каждая ошибка модели из раздела «Ошибки» даёт своё
;;;; условие с указанием компонента и формы; канонический текст модели и её
;;;; хэш одинаковы между запусками SBCL; пробная модель из нескольких
;;;; компонентов раскрывается без ошибок и интерпретируется. Пробная модель
;;;; (tests/data/trial-model.lisp) повторяет механику ручной модели, поэтому
;;;; интерпретатор сверяется с ручными ядрами.

(in-package #:worm.tests)

(def-suite dsl :in worm :description "worm.dsl.")
(in-suite dsl)

(defun trial-model ()
  (or (worm.dsl:find-model 'trial-worm nil)
      (progn (load (merge-pathnames "tests/data/trial-model.lisp" (worm.core:project-root)))
             (worm.dsl:find-model 'trial-worm))))

;;; Сверка с ручными ядрами

(defun oracle-element-arrays (set tube)
  "Жёсткости, вязкости и натяжения покоя рёбер — как в ручной модели."
  (let* ((kk (worm.topology:tube-ring-nodes tube))
         (ei (* (worm.params:param-value set :bending-ei) (worm.params:param-value set :ei-correction)))
         (soft (worm.params:param-value set :tail-softening))
         (ratios (list :circ (worm.params:param-value set :k-circ-ratio)
                       :shear (worm.params:param-value set :k-shear-ratio)
                       :diam (worm.params:param-value set :k-diam-ratio)))
         (tau (worm.params:param-value set :tau-cuticle))
         (p0 (worm.params:param-value set :prestress)))
    (multiple-value-bind (circ long diag) (worm.oracle:prestress-tensions tube)
      (loop for (family k eta tension) in '((:long-edge :k-long :eta-long :t-long)
                                            (:circ-edge :k-circ :eta-circ :t-circ)
                                            (:diagonal :k-diag :eta-diag :t-diag)
                                            (:diametral :k-diam :eta-diam :t-diam))
            for stiffness = (worm.oracle::edge-stiffness tube family ei soft ratios)
            for n = (length stiffness)
            append (list (cons k stiffness)
                         (cons eta (map 'worm.core:dvec (lambda (x) (* tau x)) stiffness))
                         (cons tension (let ((v (worm.core:make-dvec n)))
                                         (dotimes (e n v)
                                           (setf (aref v e)
                                                 (* p0 (ecase family
                                                         (:long-edge (aref long (floor e kk)))
                                                         (:circ-edge (aref circ (floor e kk)))
                                                         (:diagonal (aref diag e))
                                                         (:diametral 0d0))))))))))))

(defun perturbed (y seed scale)
  (let ((state (sb-ext:seed-random-state seed)) (out (copy-seq y)))
    (dotimes (i (length out) out) (incf (aref out i) (* scale (- (random 1d0 state) 0.5d0))))))

(defun trial-and-oracle (&key (rings 6) (kernels nil kernels-p))
  (let* ((set (worm.params:load-param-set "water-v1"))
         (body (apply #'worm.oracle:build-body :params set :rings rings
                      :amplitude (worm.params:param-value set :gen-amplitude)
                      :frequency (worm.params:param-value set :gen-frequency)
                      :wavelength (worm.params:param-value set :gen-wavelength)
                      (when kernels-p (list :kernels kernels))))
         (model (worm.dsl:instantiate (trial-model) :params set :rings rings
                                                    :element-arrays (oracle-element-arrays set (worm.oracle:body-tube body)))))
    (values model body set)))

(test trial-model-forces-match-the-oracle
  ;; Рёбра всех семейств с натяжением покоя и натяжением мышц, давление с p₀:
  ;; те же силы, что у ручных ядер, при случайно возмущённых координатах.
  (multiple-value-bind (model body) (trial-and-oracle)
    (let* ((y (perturbed (worm.oracle:body-y0 body) 3 0.004d0))
           (time 0.37d0)
           (oracle (worm.oracle:body-model body))
           (syn (worm.core:make-dvec 95))
           (g (worm.core:make-dvec (length y))))
      (funcall (worm.core:compiled-model-neural-output oracle) (worm.core:make-dvec 0)
               (worm.core:make-dvec (worm.core:compiled-model-n-observables oracle)) (worm.core:make-dvec 0)
               (worm.oracle:body-pvec body) (worm.core:dvec-from (list time 0 0)) syn)
      (funcall (worm.core:compiled-model-mech-forces oracle) y syn (worm.oracle:body-pvec body) g)
      (let* ((result (worm.dsl:interpret model y :time time))
             (f (worm.dsl:interpretation-forces result))
             (scale (reduce #'max g :key #'abs))
             (worst (loop for i below (length g) maximize (abs (- (aref g i) (aref f i))))))
        (is (> scale 1d0) "силы заметные: до ~,2F мкН" scale)
        (is (< (/ worst scale) 1d-10) "расхождение ~,2E мкН при силах до ~,2F" worst scale)
        ;; натяжение мышц — тот же выход, что у кинематики ручной модели
        (let ((force (cdr (assoc :force (cdr (assoc :drive (worm.dsl:interpretation-outputs result)))))))
          (is (< (loop for m below 95 maximize (abs (- (aref force m) (aref syn m)))) 1d-12)))
        ;; кальций: dCa/dt = (вход⁺ − Ca)/τ при Ca = 0
        (let ((dca (cdr (assoc :ca (cdr (assoc :calcium (worm.dsl:interpretation-derivs result))))))
              (tau (worm.params:param-value (worm.oracle:body-params body) :tau-ca)))
          (is (< (loop for m below 95 maximize (abs (- (aref dca m) (/ (aref syn m) tau)))) 1d-12)))))))

(test trial-model-damping-matches-the-oracle
  ;; Члены :damping (вязкость рёбер) дают ту же матрицу D, что ручная модель
  ;; при h = 0 без сопротивления среды.
  (multiple-value-bind (model body)
      (trial-and-oracle :kernels '(:long-edge :circ-edge :diagonal :diametral :pressure :tension))
    (let* ((y (perturbed (worm.oracle:body-y0 body) 5 0.004d0))
           (oracle (worm.oracle:body-model body))
           (n (length y))
           (kl (worm.core:compiled-model-lower-band oracle))
           (ab (worm.core:make-band n kl kl)))
      (funcall (worm.core:compiled-model-mech-matrix oracle) y (worm.core:make-dvec 95)
               (worm.oracle:body-pvec body) (worm.core:dvec-from '(0 0 0)) ab)
      (let ((b (band-to-dense ab n kl kl))
            (d (worm.dsl:interpret-damping model y))
            (worst 0d0) (scale 0d0))
        (dotimes (i n)
          (dotimes (j n)
            (setf worst (max worst (abs (- (aref b i j) (aref d i j))))
                  scale (max scale (abs (aref b i j))))))
        (is (< (/ worst scale) 1d-12) "расхождение D ~,2E при элементах до ~,2E" worst scale)))))

(test trial-model-expands-cleanly
  (let ((spec (trial-model)))
    (is (= 7 (length (worm.dsl:model-spec-instances spec))))
    (is (equal '("trial-calcium" "trial-cuticle-edge" "trial-kinematic-muscle" "trial-pressure")
               (mapcar (lambda (c) (string-downcase (symbol-name (worm.dsl:component-name c))))
                       (worm.dsl:model-spec-components spec))))
    (is (= 32 (length (worm.dsl:model-spec-hash spec))))
    (is (search "(:model trial-worm" (worm.dsl:model-spec-canonical-text spec)))
    (is (not (search "cl-user" (worm.dsl:model-spec-canonical-text spec))) "без имён пакетов")))

(test component-jacobian-block-matches-finite-differences
  ;; Локальный якобиан ядра (шаг 4 кодогенерации) — символьная производная
  ;; сил по координатам слотов; для ребра это блок 6 × 6.
  (let* ((kernel (worm.dsl:lower-component (worm.dsl:find-component 'trial-cuticle-edge)))
         (keys (loop for slot in '(:a :b) append (loop for c below 3 collect (list :pos slot c))))
         (state (sb-ext:seed-random-state 17))
         (point (loop for k in keys collect (cons k (+ (if (eq (second k) :b) 0.02d0 0d0) (random 0.01d0 state)))))
         (env (append point '(((:array :stiffness) . 1700d0) ((:array :rest-tension) . 0.3d0)
                              ((:input :tension) . 2d0) ((:element :rest-length) . 0.019d0)))))
    (worm.ir:with-graph ((worm.dsl:kernel-graph kernel))
      (let ((worst 0d0))
        (loop for (slot c node) in (worm.dsl:kernel-forces kernel)
              do (dolist (k keys)
                   (let* ((d (worm.ir:evaluate (worm.ir:derivative node (worm.ir:var k)) env))
                          (h 1d-8)
                          (plus (mapcar (lambda (p) (if (equal (car p) k) (cons k (+ (cdr p) h)) p)) env))
                          (minus (mapcar (lambda (p) (if (equal (car p) k) (cons k (- (cdr p) h)) p)) env))
                          (fd (/ (- (worm.ir:evaluate node plus) (worm.ir:evaluate node minus)) (* 2 h))))
                     (setf worst (max worst (/ (abs (- d fd)) (max 1d0 (abs fd))))))))
        (is (= 6 (length (worm.dsl:kernel-forces kernel))) "сила на 6 координат концов")
        (is (< worst 1d-5) "блок 6 × 6: расхождение ~,2E" worst)))))

;;; Ошибки модели

(defmacro expect-error ((type &key component form) &body body)
  "Тело даёт условие TYPE с указанием компонента и формы."
  `(handler-case (progn ,@body (fail "ожидалось условие ~S" ',type))
     (,type (e)
       (pass)
       (is (typep e 'worm.dsl:dsl-location) "условие указывает место")
       ,@(when component
           `((is (string-equal ,component (string (worm.dsl:dsl-location-component e)))
                 "компонент ~A, а не ~A" (worm.dsl:dsl-location-component e) ,component)))
       ,@(when form
           `((is (funcall ,form (worm.dsl:dsl-location-form e)) "форма ~S" (worm.dsl:dsl-location-form e))))
       e)))

(defun mentions (fragment)
  (lambda (form) (search fragment (string-downcase (format nil "~S" form)))))

(defun trial-model-form (&key (components nil components-p) (wiring nil wiring-p)
                              (anatomy "anatomy/muscles.sexp"))
  "Форма пробной модели с заменёнными частями."
  (let* ((form (worm.dsl::model-spec-source (trial-model)))
         (plist (copy-list (cddr form))))
    (when components-p (setf (getf plist :components) components))
    (when wiring-p (setf (getf plist :wiring) wiring))
    (setf (getf plist :anatomy) anatomy)
    (list* (first form) (second form) plist)))

(defun trial-components-with (name replacement)
  "Экземпляры пробной модели, где экземпляр NAME заменён REPLACEMENT."
  (mapcar (lambda (c) (if (string-equal (symbol-name (first c)) name) replacement c))
          (getf (cddr (worm.dsl::model-spec-source (trial-model))) :components)))

(test unbound-input-is-reported
  (trial-model)
  (let ((e (expect-error (worm.dsl:unbound-input :component "calcium" :form (mentions "trial-calcium"))
             (worm.dsl:parse-model (trial-model-form :wiring '(((drive force) (cuticle-long tension) :via :attachment)))))))
    (is (eq :drive (worm.dsl::unbound-input-input e)))))

(test input-bound-twice-is-reported
  (trial-model)
  (expect-error (worm.dsl:input-bound-twice :component "calcium" :form (mentions ":identity"))
    (worm.dsl:parse-model (trial-model-form :wiring '(((drive force) (cuticle-long tension) :via :attachment)
                                                       ((drive force) (calcium drive) :via :identity)
                                                       ((drive force) (calcium drive) :via :identity))))))

(worm.dsl:define-component test-relay
  :family :muscle
  :inputs ((in (link :signal)))
  :outputs ((out (* 2 in))))

(test algebraic-loop-is-reported
  (let ((e (expect-error (worm.dsl:algebraic-loop :form (lambda (f) (eq f :neural)))
             (worm.dsl:parse-model '(defmodel looping
                                      :topology (tube :rings 6 :ring-nodes 8)
                                      :anatomy "anatomy/muscles.sexp"
                                      :components ((first-relay test-relay :on :muscle)
                                                   (second-relay test-relay :on :muscle))
                                      :wiring (((first-relay out) (second-relay in) :via :identity)
                                               ((second-relay out) (first-relay in) :via :identity))
                                      :subsystems ((:neural first-relay second-relay)))))))
    (is (equal '("first-relay.in" "first-relay.out" "second-relay.in" "second-relay.out" "first-relay.in")
               (worm.dsl:algebraic-loop-cycle e))
        "петля названа по портам: ~S" (worm.dsl:algebraic-loop-cycle e)))
  ;; та же связь через переменную состояния — не петля
  (finishes (worm.dsl:parse-model (trial-model-form))))

(test family-mismatch-is-reported
  (trial-model)
  (expect-error (worm.dsl:family-mismatch :component "calcium" :form (mentions ":long-edge"))
    (worm.dsl:parse-model (trial-model-form :components (trial-components-with
                                                         "calcium" '(calcium trial-calcium :on :long-edge)))))
  (expect-error (worm.dsl:family-mismatch :component "shape" :form (mentions ":attachment"))
    (worm.dsl:parse-model (trial-model-form :wiring '(((drive force) (shape tension) :via :attachment)
                                                       ((drive force) (calcium drive) :via :identity)))))
  (expect-error (worm.dsl:family-mismatch :component "sideways" :form (mentions ":sideways"))
    (worm.dsl:parse-component '(define-component sideways :family :sideways))))

(test unknown-parameter-is-reported
  (trial-model)
  (let ((e (expect-error (worm.params:unknown-parameter :component "pressure" :form (mentions "bulk-modulus"))
             (worm.dsl:parse-model
              (trial-model-form :components
                                (trial-components-with
                                 "pressure" '(pressure trial-pressure :on :compartment
                                              :rename (bulk-modulus bulk-modulus-typo))))))))
    (is (typep e 'worm.dsl:model-unknown-parameter))))

(test anatomy-problems-are-reported
  (trial-model)
  (let ((broken (merge-pathnames "cache/test-dsl/muscles.sexp" (worm.core:project-root))))
    (ensure-directories-exist broken)
    (with-open-file (out broken :direction :output :if-exists :supersede)
      (write-line "(muscles (:name :mdl01 :quadrant :dorsal-left :row :medial :index 1 :from 0.5 :peak 0.4 :to 0.3))" out))
    (expect-error (worm.anatomy:anatomy-invalid :form (mentions "cache/test-dsl"))
      (worm.dsl:parse-model (trial-model-form :anatomy "cache/test-dsl/muscles.sexp")))
    (expect-error (worm.anatomy:anatomy-invalid :form (mentions "no-such-file"))
      (worm.dsl:parse-model (trial-model-form :anatomy "anatomy/no-such-file.sexp")))))

(test ir-type-error-is-reported
  (expect-error (worm.ir:ir-type-error :component "bad-mix" :form (mentions "(+ edge-vector 1)"))
    (worm.dsl:parse-component '(define-component bad-mix :family :edge
                                :outputs ((x (+ edge-vector 1))))))
  (expect-error (worm.ir:ir-type-error :component "bad-power" :form (mentions "expt"))
    (worm.dsl:parse-component '(define-component bad-power :family :edge
                                :outputs ((x (expt edge-length 0.5))))))
  (expect-error (worm.ir:ir-type-error :component "bad-arity" :form (mentions "hill"))
    (worm.dsl:parse-component '(define-component bad-arity :family :muscle
                                :params (tau-ca) :outputs ((x (hill tau-ca 1))))))
  (expect-error (worm.dsl:component-ir-type-error :component "bad-force" :form (mentions "(pos a)"))
    (worm.dsl:parse-component '(define-component bad-force :family :edge
                                :forces (((pos a) edge-length))))))

(worm.ir:define-primitive test-opaque (x)
  :value (lambda (x) (* x x x))
  :documentation "Примитив без производной.")

(test no-derivative-is-reported
  (let ((e (expect-error (worm.ir:no-derivative :component "opaque-force" :form (mentions ":gradient"))
             (worm.dsl:parse-component '(define-component opaque-force :family :edge
                                         :forces ((:gradient (test-opaque edge-length) :scale 1)))))))
    (is (string= "TEST-OPAQUE" (symbol-name (worm.ir:no-derivative-primitive e))))
    (is (typep e 'worm.dsl:component-no-derivative)))
  ;; вычислять примитив без производной можно
  (finishes (worm.dsl:parse-component '(define-component opaque-output :family :edge
                                        :outputs ((x (test-opaque edge-length)))))))

(test syntax-and-name-errors-are-reported
  (expect-error (worm.dsl:unknown-name :component "typo" :form (mentions "edge-lenght"))
    (worm.dsl:parse-component '(define-component typo :family :edge :outputs ((x (* 2 edge-lenght))))))
  (expect-error (worm.dsl:dsl-syntax-error :component "bad-op" :form (mentions "sinh"))
    (worm.dsl:parse-component '(define-component bad-op :family :edge :outputs ((x (sinh edge-length))))))
  (expect-error (worm.dsl:dsl-syntax-error :component "bad-key" :form (lambda (f) (eq f :force)))
    (worm.dsl:parse-component '(define-component bad-key :family :edge :force ())))
  (expect-error (worm.dsl:dsl-syntax-error :component "rate-outside" :form (mentions "length-rate"))
    (worm.dsl:parse-component '(define-component rate-outside :family :edge
                                :outputs ((x (* 2 length-rate))))))
  (expect-error (worm.dsl:dsl-syntax-error :component "nonlinear-damping" :form (mentions "length-rate"))
    (worm.dsl:parse-component '(define-component nonlinear-damping :family :edge
                                :damping ((1 (* length-rate length-rate))))))
  (expect-error (worm.dsl:dsl-syntax-error :component "no-deriv" :form (mentions "ca 0"))
    (worm.dsl:parse-component '(define-component no-deriv :family :muscle :state ((ca 0)))))
  (expect-error (worm.dsl:unknown-component :component "drive" :form (mentions "no-such-component"))
    (worm.dsl:parse-model (trial-model-form :components (trial-components-with
                                                         "drive" '(drive no-such-component :on :muscle)))))
  ;; lagged разрешает скорость прошлого шага
  (finishes (worm.dsl:parse-component '(define-component lagged-rate :family :edge
                                        :outputs ((x (* 2 (lagged length-rate))))))))

;;; Канонический текст и хэш между запусками

(test canonical-text-and-hash-are-stable-between-runs
  (let* ((spec (trial-model))
         (output (with-output-to-string (s)
                   (sb-ext:run-program "sbcl"
                                       (list "--non-interactive" "--no-userinit"
                                             "--load" (namestring (merge-pathnames "scripts/init.lisp" (worm.core:project-root)))
                                             "--eval" "(asdf:load-system \"worm/dsl\")"
                                             "--load" (namestring (merge-pathnames "tests/data/trial-model.lisp" (worm.core:project-root)))
                                             "--eval" "(let ((m (worm.dsl:find-model 'cl-user::trial-worm))) (format t \"~&HASH ~A ~D~%\" (worm.dsl:model-spec-hash m) (length (worm.dsl:model-spec-canonical-text m))))")
                                       :search t :output s :error nil
                                       :directory (namestring (worm.core:project-root)))))
         (line (find-if (lambda (l) (and (> (length l) 5) (string= "HASH " l :end2 5)))
                        (uiop:split-string output :separator '(#\Newline)))))
    (is (equal (format nil "HASH ~A ~D" (worm.dsl:model-spec-hash spec)
                       (length (worm.dsl:model-spec-canonical-text spec)))
               line)
        "в другом процессе: ~S" line)
    ;; и повторное раскрытие в этом процессе
    (is (string= (worm.dsl:model-spec-hash spec)
                 (worm.dsl:model-spec-hash (worm.dsl:parse-model (worm.dsl::model-spec-source spec)))))))

(test volume-primitive-derivatives-match-finite-differences
  ;; volume → volume-grad → volume-hess: производные примитивов объёма
  ;; (решение 21) против конечных разностей для отсека с K = 4.
  (let* ((tube (worm.topology:make-tube :rings 2 :ring-nodes 4))
         (y (perturbed (worm.topology:tube-rest-coords tube) 23 0.01d0))
         (worst-grad 0d0) (worst-hess 0d0))
    (worm.ir:with-graph ()
      (let* ((vars (loop for i below 24 collect (worm.ir:var (list :x i))))
             (env (loop for i below 24 collect (cons (list :x i) (aref y i))))
             (v (worm.ir:prim 'worm.dsl::volume vars)))
        (is (worm.core:approx= (worm.ir:evaluate v env) (worm.topology:compartment-volume y 0 4) :rel 1d-12))
        (flet ((shifted (i h) (mapcar (lambda (p) (if (equal (car p) (list :x i)) (cons (car p) (+ (cdr p) h)) p)) env)))
          (loop for i below 24 for xi in vars
                do (let* ((g (worm.ir:derivative v xi))
                          (fd (/ (- (worm.ir:evaluate v (shifted i 1d-6)) (worm.ir:evaluate v (shifted i -1d-6))) 2d-6)))
                     (setf worst-grad (max worst-grad (abs (- fd (worm.ir:evaluate g env)))))
                     (loop for j in '(0 5 13 23)
                           do (let* ((hj (worm.ir:derivative g (nth j vars)))
                                     (fdh (/ (- (worm.ir:evaluate g (shifted j 1d-6)) (worm.ir:evaluate g (shifted j -1d-6))) 2d-6)))
                                (setf worst-hess (max worst-hess (abs (- fdh (worm.ir:evaluate hj env))))))))))))
    (is (< worst-grad 1d-9) "градиент: ~,2E" worst-grad)
    (is (< worst-hess 1d-7) "гессиан: ~,2E" worst-hess)))

;;;; src/dsl/lower.lisp — понижение выражений компонента в IR.
;;;;
;;;; Архитектура, «Кодогенерация», шаг 2: ядро компонента на семействе
;;;; становится графом скалярных узлов. Векторные операции раскладываются на
;;;; три компоненты, геометрия — на чтения координат слотов, параметры и
;;;; входы — на переменные. Переменные (листья) задаются относительно
;;;; элемента; какие числа за ними стоят, решает тот, кто исполняет ядро:
;;;;
;;;;   (:pos слот c)   координата c узла слота               y-mech
;;;;   (:vel слот c)   скорость — только в :damping           (матрица D)
;;;;   (:aux слот c)   скорость прошлого шага — внутри lagged  aux
;;;;   (:param имя)    скалярный параметр (имя после переименования)
;;;;   (:array имя)    поэлементный параметр
;;;;   (:state имя)    непрерывное состояние элемента
;;;;   (:discrete имя) дискретное состояние элемента
;;;;   (:input имя)    связанный вход
;;;;   (:element имя)  величина покоя или положения элемента (rest-length,
;;;;                   rest-volume, ds, position, side, rest-radius)
;;;;   (:time)         время
;;;;
;;;; Имена языка сравниваются по имени символа, а не по пакету: компоненты
;;;; пишутся в своих пакетах. Имена компонента (параметры, состояние, входы,
;;;; let) — ключевыми словами.

(in-package #:worm.dsl)


;;; Силы из :gradient, собираемые LOWER-FORCES для кодогенерации.
(defvar *gradients* nil)

;;; Окружение понижения

(defstruct (lenv (:copier nil))
  component family ring-nodes
  (bindings (make-hash-table :test 'eq))   ; ключ имени → значение (узел или список из трёх)
  (rate-mode :forbid)                        ; :forbid :lagged :damping
  (geometry (make-hash-table :test 'eq)))    ; запомненная геометрия по режиму

(defun scalar-p (v) (worm.ir:node-p v))
(defun vector-p (v) (and (consp v) (= (length v) 3) (every #'worm.ir:node-p v)))

(defun need-scalar (v what)
  (unless (scalar-p v) (dsl-error 'component-ir-type-error "~A: ожидался скаляр, а получен вектор" what))
  v)

(defun need-vector (v what)
  (unless (vector-p v) (dsl-error 'component-ir-type-error "~A: ожидался вектор, а получен скаляр" what))
  v)

(defun leaf (&rest key) (worm.ir:var key))

(defun slot-position (slot) (loop for c below 3 collect (leaf :pos slot c)))

(defun slot-rate (env slot)
  (ecase (lenv-rate-mode env)
    (:forbid (dsl-error 'dsl-syntax-error
                        "скорость узлов входит в выражения только через (lagged …) или в :damping"))
    (:lagged (loop for c below 3 collect (leaf :aux slot c)))
    (:damping (loop for c below 3 collect (leaf :vel slot c)))))

;;; Векторная арифметика над тройками узлов

(defun v+ (a b) (mapcar #'ir+ a b))
(defun v- (a b) (mapcar #'ir- a b))
(defun vscale (s v) (mapcar (lambda (x) (ir* s x)) v))
(defun vdot (a b) (apply #'ir+ (mapcar #'ir* a b)))
(defun vcross (a b)
  (destructuring-bind (a1 a2 a3) a
    (destructuring-bind (b1 b2 b3) b
      (list (ir- (ir* a2 b3) (ir* a3 b2))
            (ir- (ir* a3 b1) (ir* a1 b3))
            (ir- (ir* a1 b2) (ir* a2 b1))))))
(defun vnorm (a) (worm.ir:ir-sqrt (vdot a a)))
(defun vunit (a) (let ((n (vnorm a))) (mapcar (lambda (x) (ir/ x n)) a)))

;;; Геометрия семейств

(defun geometry-value (env name)
  "Значение геометрического имени NAME (строка) для семейства окружения."
  (let ((family (lenv-family env))
        (key (cons name (lenv-rate-mode env))))
    (or (gethash key (lenv-geometry env))
        (setf (gethash key (lenv-geometry env))
              (cond
                ((edge-family-p family)
                 (let ((ev (v- (slot-position :b) (slot-position :a))))
                   (cond ((string= name "EDGE-VECTOR") ev)
                         ((string= name "EDGE-LENGTH") (vnorm ev))
                         ((string= name "REST-LENGTH") (leaf :element :rest-length))
                         ((string= name "SIDE") (leaf :element :side))
                         ((string= name "RADIUS-FACTOR") (leaf :element :radius-factor))
                         ((string= name "LENGTH-RATE")
                          (vdot (vunit ev) (v- (slot-rate env :b) (slot-rate env :a)))))))
                ((eq family :compartment)
                 (cond ((string= name "VOLUME")
                        (worm.ir:prim 'volume (loop for j below (* 2 (lenv-ring-nodes env))
                                                    append (slot-position j))))
                       ((string= name "REST-VOLUME") (leaf :element :rest-volume))))
                ((eq family :ring)
                 (cond ((string= name "CENTROID")
                        (let ((k (lenv-ring-nodes env)))
                          (loop for c below 3
                                collect (ir/ (apply #'ir+ (loop for j below k collect (leaf :pos j c))) k))))
                       ((string= name "DS") (leaf :element :ds))
                       ((string= name "REST-RADIUS") (leaf :element :rest-radius))
                       ((string= name "POSITION") (leaf :element :position))))
                ((eq family :node)
                 (cond ((string= name "DS") (leaf :element :ds))
                       ((string= name "POSITION") (leaf :element :position))
                       ((string= name "TANGENT")
                        ;; касательная по центрам соседних колец, как у ручной модели
                        (let* ((k (lenv-ring-nodes env))
                               (prev (loop for c below 3
                                           collect (ir/ (apply #'ir+ (loop for j from 1 to k collect (leaf :pos j c))) k)))
                               (next (loop for c below 3
                                           collect (ir/ (apply #'ir+ (loop for j from (1+ k) to (* 2 k)
                                                                           collect (leaf :pos j c)))
                                                        k))))
                          (vunit (v- next prev))))))
                ((eq family :muscle)
                 (cond ((string= name "POSITION") (leaf :element :position))
                       ((string= name "SIDE") (leaf :element :side))))
                ((eq family :prop-unit)
                 (cond ((string= name "POSITION") (leaf :element :position))
                       ((string= name "INDEX") (leaf :element :index)))))))))

(defun geometry-name-p (env symbol)
  (member (symbol-name symbol) (family-geometry (lenv-family env)) :key #'symbol-name :test #'string=))

;;; Выражения

(defparameter +scalar-functions+
  `(("SQRT" . worm.ir:ir-sqrt) ("EXP" . worm.ir:ir-exp) ("LOG" . worm.ir:ir-log)
    ("TANH" . worm.ir:ir-tanh) ("SIN" . worm.ir:ir-sin) ("COS" . worm.ir:ir-cos)
    ("LOGISTIC" . worm.ir:logistic)))

(defparameter +operators+
  '("+" "-" "*" "/" "EXPT" "SQRT" "EXP" "LOG" "TANH" "SIN" "COS" "LOGISTIC" "HILL" "POSITIVE" "CLAMP"
    "VEC" "DOT" "CROSS" "NORM" "UNIT" "SCALE" "V+" "V-" "POS" "VELOCITY" "TIME" "LAGGED")
  "Операции языка выражений (архитектура, «Язык выражений»).")

(defun lower-expr (env form)
  "Узел IR (скаляр) или список из трёх узлов (вектор)."
  (let ((*form* (if (consp form) form *form*)))
    (cond
      ((realp form) (worm.ir:const form))
      ((name-is form "PI") (worm.ir:const pi))
      ((symbolp form)
       (multiple-value-bind (v found) (gethash (kw form) (lenv-bindings env))
         (cond (found v)
               ((geometry-name-p env form) (geometry-value env (symbol-name form)))
               (t (dsl-error 'unknown-name "имя ~(~A~) не определено в компоненте (семейство ~(~A~))"
                             form (lenv-family env))))))
      ((and (consp form) (symbolp (first form)))
       (lower-call env (symbol-name (first form)) (rest form) form))
      (t (dsl-error 'dsl-syntax-error "не выражение: ~S" form)))))

(defun lower-call (env op args form)
  (flet ((scalars () (mapcar (lambda (a) (need-scalar (lower-expr env a) op)) args))
         (vectors () (mapcar (lambda (a) (need-vector (lower-expr env a) op)) args))
         (arity (n) (unless (= (length args) n)
                      (dsl-error 'component-ir-type-error "~(~A~) ждёт ~D аргументов, а получил ~D"
                                 op n (length args)))))
    (cond
      ((string= op "+") (apply #'ir+ (scalars)))
      ((string= op "-") (if args (apply #'ir- (scalars))
                            (dsl-error 'component-ir-type-error "(-) без аргументов")))
      ((string= op "*") (apply #'ir* (scalars)))
      ((string= op "/") (arity 2) (apply #'ir/ (scalars)))
      ((string= op "EXPT") (arity 2)
       (unless (integerp (second args))
         (dsl-error 'component-ir-type-error "expt — только с целой степенью, а не ~S" (second args)))
       (worm.ir:ir-expt (need-scalar (lower-expr env (first args)) op) (second args)))
      ((assoc op +scalar-functions+ :test #'string=)
       (arity 1) (funcall (cdr (assoc op +scalar-functions+ :test #'string=)) (first (scalars))))
      ((string= op "HILL") (arity 3) (worm.ir:prim 'worm.ir:hill (scalars)))
      ((string= op "POSITIVE") (arity 1) (worm.ir:prim 'worm.ir:positive (scalars)))
      ((string= op "CLAMP") (arity 3) (worm.ir:prim 'worm.ir:clamp (scalars)))
      ((string= op "VEC") (arity 3) (scalars))
      ((string= op "DOT") (arity 2) (apply #'vdot (vectors)))
      ((string= op "CROSS") (arity 2) (apply #'vcross (vectors)))
      ((string= op "NORM") (arity 1) (vnorm (first (vectors))))
      ((string= op "UNIT") (arity 1) (vunit (first (vectors))))
      ((string= op "SCALE") (arity 2)
       (vscale (need-scalar (lower-expr env (first args)) op) (need-vector (lower-expr env (second args)) op)))
      ((string= op "V+") (arity 2) (apply #'v+ (vectors)))
      ((string= op "V-") (case (length args)
                           (1 (vscale -1d0 (first (vectors))))
                           (2 (apply #'v- (vectors)))
                           (t (arity 2))))
      ((string= op "POS")
       (let ((slot (slot-designator (lenv-family env) form)))
         (unless slot
           (dsl-error 'unknown-name "у семейства ~(~A~) нет слота ~S" (lenv-family env) (first args)))
         (when (and (integerp slot) (>= slot (length (family-slots (lenv-family env) (lenv-ring-nodes env)))))
           (dsl-error 'unknown-name "у семейства ~(~A~) нет слота ~D" (lenv-family env) slot))
         (slot-position slot)))
      ((string= op "VELOCITY")
       ;; скорость узла слота: в :damping — скорость шага, внутри lagged — прошлого
       (let ((slot (slot-designator (lenv-family env) (cons 'pos args))))
         (unless slot
           (dsl-error 'unknown-name "у семейства ~(~A~) нет слота ~S" (lenv-family env) (first args)))
         (slot-rate env slot)))
      ((string= op "TIME") (arity 0) (leaf :time))
      ((string= op "LAGGED") (arity 1)
       (let ((saved (lenv-rate-mode env)))
         (setf (lenv-rate-mode env) :lagged)
         (unwind-protect (lower-expr env (first args))
           (setf (lenv-rate-mode env) saved))))
      ((user-primitive op)
       ;; примитив, определённый DEFINE-PRIMITIVE, — часть языка выражений
       (worm.ir:prim (worm.ir:primitive-name (user-primitive op)) (scalars)))
      (t (dsl-error 'dsl-syntax-error "в языке выражений нет операции ~(~A~)" op)))))

(defun user-primitive (op)
  "Примитив с именем OP (строка), кроме служебных примитивов объёма и Хилла."
  (let ((found nil))
    (maphash (lambda (name p)
               (when (and (string= (symbol-name name) op)
                          (not (member op '("VOLUME" "VOLUME-GRAD" "VOLUME-HESS" "HILL-DX") :test #'string=)))
                 (setf found p)))
             worm.ir::*primitives*)
    found))

;;; Ядро компонента

(defstruct (kernel (:copier nil))
  component instance family ring-nodes graph
  outputs        ; ((имя . узел) …)
  forces         ; ((слот c узел) …) — сила на координату c узла слота
  derivs         ; ((состояние . узел) …)
  damping        ; ((коэффициент . скорость) …) — функция Рэлея ½·Σ коэф·скорость²
  state-inits    ; ((состояние . узел) …)
  checks         ; ((причина . узел) …) — сбой, если значение отрицательно
  gradients      ; ((скаляр коэффициент номера-сил) …) — силы вида k·∂S/∂x из :gradient
  output-deps    ; ((выход . (входы …)) …) — от каких входов выход зависит мгновенно
  events)        ; ((:rising|:falling переменная условие значение) …)

(defun kernel-roots (kernel)
  "Все корни ядра для вычислителя."
  (append (mapcar #'cdr (kernel-outputs kernel))
          (mapcar #'third (kernel-forces kernel))
          (mapcar #'cdr (kernel-derivs kernel))
          (loop for (nil nil c v) in (kernel-events kernel) append (list c v))))

(defun kernel-leaves (kernel)
  "Ключи всех переменных ядра, в печатном порядке."
  (let ((keys '()) (seen (make-hash-table)))
    (labels ((walk (m)
               (unless (gethash (worm.ir:node-id m) seen)
                 (setf (gethash (worm.ir:node-id m) seen) t)
                 (if (worm.ir:var-p m)
                     (push (worm.ir:var-key m) keys)
                     (loop for a across (worm.ir:node-args m) do (walk a))))))
      (mapc #'walk (kernel-roots kernel)))
    (sort keys #'string< :key (lambda (k) (format nil "~S" k)))))

(defun input-dependencies (node)
  "Имена входов, от которых зависит узел."
  (let ((seen (make-hash-table)) (out '()))
    (labels ((walk (m)
               (unless (gethash (worm.ir:node-id m) seen)
                 (setf (gethash (worm.ir:node-id m) seen) t)
                 (if (worm.ir:var-p m)
                     (let ((k (worm.ir:var-key m))) (when (eq (first k) :input) (push (second k) out)))
                     (loop for a across (worm.ir:node-args m) do (walk a))))))
      (walk node))
    (sort out #'string< :key #'symbol-name)))

(defun lower-component (component &key family (ring-nodes 8) renames instance)
  "Ядро COMPONENT на семействе FAMILY (по умолчанию — его собственном;
для :edge — продольные рёбра). RENAMES — ((имя-компонента . имя-модели) …)."
  (let* ((family (or family (if (eq (component-family component) :edge) :long-edge (component-family component))))
         (graph (worm.ir:make-graph))
         (env (make-lenv :component component :family family :ring-nodes ring-nodes)))
    (with-location (:component (or instance (component-name component)) :form nil)
      (let ((*gradients* '()))
      (worm.ir:with-graph (graph)
        (flet ((bind (name value)
                 (setf (gethash name (lenv-bindings env)) value))
               (lower (form) (let ((*form* form)) (lower-expr env form))))
          (dolist (p (component-params component))
            (destructuring-bind (name kind) p
              (let ((global (or (cdr (assoc name renames)) name)))
                (bind name (if (eq kind :per-element) (leaf :array global) (leaf :param global))))))
          (dolist (s (component-state component)) (bind (first s) (leaf :state (first s))))
          (dolist (d (component-discrete component)) (bind (first d) (leaf :discrete (first d))))
          (dolist (i (component-inputs component)) (bind (first i) (leaf :input (first i))))
          (dolist (l (component-lets component))
            (let ((*form* (second l)))
              (bind (first l) (lower (second l)))))
          (let ((outputs (loop for (name form) in (component-outputs component)
                               collect (cons name (let ((*form* form)) (need-scalar (lower form) "выход")))))
                (derivs (loop for (name form) in (component-derivs component)
                              collect (cons name (let ((*form* form)) (need-scalar (lower form) "производная")))))
                (forces (lower-forces env component #'lower))
                (damping (lower-damping env component #'lower))
                (inits (loop for (name form) in (component-state component)
                             collect (cons name (let ((*form* form)) (need-scalar (lower form) "начальное значение")))))
                (checks (loop for (cause form) in (component-checks component)
                              collect (cons cause (let ((*form* form)) (need-scalar (lower form) "проверка")))))
                (events (lower-events component #'lower)))
            (make-kernel :component component :instance instance :family family :ring-nodes ring-nodes
                         :graph graph :outputs outputs :derivs derivs :forces forces :damping damping
                         :state-inits inits :checks checks :gradients (reverse *gradients*) :events events
                         :output-deps (loop for (name . node) in outputs
                                            collect (cons name (input-dependencies node)))))))))))

(defun lower-events (component lower)
  "События: (<условие> :rising|:falling (set <дискретная переменная> <значение>)).
Проверяются на границе шага: :rising — когда условие > 0, :falling — когда < 0."
  (loop for e in (component-events component)
        collect (let ((*form* e))
                  (destructuring-bind (condition direction action) e
                    (unless (and (consp action) (= (length action) 3) (name-is (first action) "SET")
                                 (symbolp (second action))
                                 (assoc (kw (second action)) (component-discrete component)))
                      (dsl-error 'dsl-syntax-error "действие события — (set <дискретная переменная> <значение>)"))
                    (list direction (kw (second action))
                          (need-scalar (funcall lower condition) "условие события")
                          (need-scalar (funcall lower (third action)) "значение события"))))))

(defun lower-forces (env component lower)
  (let ((family (lenv-family env)) (out '()) (count 0))
    (dolist (f (component-forces component) (nreverse out))
      (let ((*form* f))
        (cond
          ;; (:gradient <скаляр> :scale <коэффициент>) — сила на каждую координату
          ;; каждого слота: коэффициент × ∂скаляр/∂координата
          ((and (consp f) (eq (first f) :gradient))
           (destructuring-bind (expr &key (scale 1)) (rest f)
             (let ((s (need-scalar (funcall lower expr) ":gradient"))
                   (k (need-scalar (funcall lower scale) ":scale"))
                   (indices '()))
               (dolist (slot (family-slots family (lenv-ring-nodes env)))
                 (dotimes (c 3)
                   (let ((d (worm.ir:derivative s (leaf :pos slot c))))
                     (unless (worm.ir:zero-p d)
                       (push count indices) (incf count)
                       (push (list slot c (ir* k d)) out)))))
               (push (list s k (nreverse indices)) *gradients*))))
          ((and (consp f) (= (length f) 2) (slot-designator family (first f)))
           (let ((slot (slot-designator family (first f)))
                 (v (need-vector (funcall lower (second f)) "сила")))
             (loop for c below 3 for node in v
                   unless (worm.ir:zero-p node) do (push (list slot c node) out) (incf count))))
          (t (dsl-error 'dsl-syntax-error "сила задаётся как ((pos <слот>) <вектор>) или (:gradient <скаляр> :scale <к>)")))))))

(defun lower-damping (env component lower)
  (loop for d in (component-damping component)
        collect (let ((*form* d))
                  (unless (and (consp d) (= (length d) 2))
                    (dsl-error 'dsl-syntax-error "член :damping — (<коэффициент> <линейная скорость>)"))
                  (let ((coef (need-scalar (funcall lower (first d)) "коэффициент вязкости"))
                        (rate (let ((saved (lenv-rate-mode env)))
                                (setf (lenv-rate-mode env) :damping)
                                (unwind-protect (need-scalar (funcall lower (second d)) "скорость")
                                  (setf (lenv-rate-mode env) saved)))))
                    (check-linear-rate rate)
                    (cons coef rate)))))

(defun velocity-leaves (node)
  (let ((out '()) (seen (make-hash-table)))
    (labels ((walk (m)
               (unless (gethash (worm.ir:node-id m) seen)
                 (setf (gethash (worm.ir:node-id m) seen) t)
                 (if (worm.ir:var-p m)
                     (when (eq (first (worm.ir:var-key m)) :vel) (push m out))
                     (loop for a across (worm.ir:node-args m) do (walk a))))))
      (walk node))
    out))

(defun check-linear-rate (rate)
  "Скорость в :damping должна зависеть от скоростей узлов и быть по ним линейной."
  (let ((vels (velocity-leaves rate)))
    (unless vels (dsl-error 'dsl-syntax-error "в :damping скорость не зависит от скоростей узлов"))
    (dolist (v vels)
      (when (velocity-leaves (worm.ir:derivative rate v))
        (dsl-error 'dsl-syntax-error "скорость в :damping нелинейна по скоростям узлов")))))

;;;; src/codegen/compile.lisp — сборка compiled-model из model-spec.
;;;;
;;;; Раскладка (архитектура, «Векторы состояния», решение 22):
;;;;   y-mech   — координаты узлов;
;;;;   y-neural — состояния экземпляров нервно-мышечной части: по экземплярам,
;;;;              внутри — по переменным, внутри — по элементам;
;;;;   aux      — скорости y-mech прошлого шага, если их читает хоть одно ядро;
;;;;   obs      — входы нервно-мышечной части, которые приходят от механики
;;;;              (уже сведённые отношениями), блоками;
;;;;   syn      — входы механики от нервно-мышечной части, блоками;
;;;;   p        — скалярные параметры модели по алфавиту.
;;;; Связи внутри одной подсистемы сводятся в рабочих буферах той функции,
;;;; где считаются. Матрица шага: локальный якобиан каждой силы по координатам
;;;; своих слотов (−h·∂F/∂x) и вязкость Σ c·g gᵀ рассеиваются по картам
;;;; индексов ленты; ширина ленты — по фактическим индексам карт.

(in-package #:worm.codegen)

(defparameter +codegen-version+ 5)

(defvar *units* nil "Единицы собираемой модели — для сведения входов.")
(defvar *relations* nil "Связи собираемой модели: ((провод . csr) …).")
(defvar *env* nil "Окружение фабрики, которая сейчас собирается.")

(defstruct (unit (:copier nil))
  running name family count count-sym nslots slots-sym
  mech-p
  outputs        ; ((порт . символ буфера) …) — выходы, у которых есть связи
  inputs         ; ((порт . источник) …): (:obs off-sym) (:syn off-sym) (:buffer sym) (:default число)
  states         ; ((переменная . off-sym) …)
  discretes      ; ((дискретная переменная . off-sym) …) — смещения в z
  syms)          ; ((ключ . символ) …) — поэлементные массивы и величины

(defstruct (model-layout (:copier nil))
  instance units param-names param-defaults neural-states obs-blocks syn-blocks
  y0 yn0 kl ku notes forms medium
  discrete-states z0     ; раскладка z: ((экземпляр переменная смещение число) …) и начальные значения
  activation)            ; (yn obs z p clock out) — активации мышц для кадров или NIL

(defun running-name (r) (worm.dsl::instance-name (worm.dsl::running-instance r)))

(defun kernel-of (u) (worm.dsl::running-kernel (unit-running u)))

;;; План

(defun plan-model (model env set)
  "Единицы (экземпляры) с окружением и раскладкой векторов."
  (let* ((mech-names (worm.dsl::model-instance-mechanical model))
         (runnings (worm.dsl::model-instance-running model))
         (relations (worm.dsl::model-instance-relations model))
         (units
           (loop for r in runnings
                 for inst = (worm.dsl::running-instance r)
                 for slots = (worm.dsl::running-slots r)
                 for nslots = (if slots (length (aref slots 0)) 0)
                 collect (make-unit
                          :running r :name (worm.dsl::instance-name inst) :family (worm.dsl::instance-family inst)
                          :count (worm.dsl::running-count r)
                          :count-sym (env-add env (worm.dsl::running-count r) :fixnum)
                          :nslots nslots
                          :slots-sym (when slots
                                       (env-add env (let ((v (make-array (* (length slots) nslots) :element-type 'fixnum)))
                                                      (dotimes (e (length slots) v)
                                                        (dotimes (j nslots) (setf (aref v (+ (* e nslots) j)) (aref (aref slots e) j)))))
                                                :ivec))
                          :mech-p (member (worm.dsl::instance-name inst) mech-names)
                          :syms (append
                                 (loop for (name . v) in (worm.dsl::running-elements r)
                                       collect (cons (list :element name) (env-add env (dvec-from v) :dvec)))
                                 (loop for (name . v) in (worm.dsl::running-arrays r)
                                       collect (cons (list :array name) (env-add env (dvec-from v) :dvec)))))))
         (param-names (sort (remove-duplicates (loop for r in runnings append (mapcar #'car (worm.dsl::running-params r))))
                            #'string< :key #'symbol-name))
         (obs-size 0) (syn-size 0) (obs-blocks '()) (syn-blocks '()) (state-offset 0) (neural-states '())
         (discrete-offset 0) (discrete-states '()))
    (flet ((unit-named (name) (find name units :key #'unit-name)))
      ;; выходы со связями — в буферы
      (loop for (w) in relations
            for src = (unit-named (worm.dsl::wire-src-instance w))
            unless (assoc (worm.dsl::wire-src-port w) (unit-outputs src))
              do (push (cons (worm.dsl::wire-src-port w) (env-add env (make-dvec (unit-count src)) :dvec))
                       (unit-outputs src)))
      ;; источники входов
      (loop for (w . csr) in relations
            for src = (unit-named (worm.dsl::wire-src-instance w))
            for dst = (unit-named (worm.dsl::wire-dst-instance w))
            for port = (worm.dsl::wire-dst-port w)
            for same = (eq (and (unit-mech-p src) t) (and (unit-mech-p dst) t))
            do (let ((csr-syms (let* ((n (length csr))
                                      (off (make-array (1+ n) :element-type 'fixnum))
                                      (total (loop for row across csr sum (length row)))
                                      (srcs (make-array total :element-type 'fixnum))
                                      (ws (make-dvec total)) (q 0))
                                 (dotimes (e n)
                                   (setf (aref off e) q)
                                   (loop for (s . wt) in (aref csr e)
                                         do (setf (aref srcs q) s (aref ws q) (float wt 0d0)) (incf q)))
                                 (setf (aref off n) q)
                                 (list (env-add env off :ivec) (env-add env srcs :ivec) (env-add env ws :dvec)))))
                 (push (cons port
                             (cond (same (list :buffer (env-add env (make-dvec (unit-count dst)) :dvec) w csr-syms))
                                   ((unit-mech-p dst)
                                    (prog1 (list :syn (env-add env syn-size :fixnum) w csr-syms)
                                      (push (list (unit-name dst) port syn-size (unit-count dst)) syn-blocks)
                                      (incf syn-size (unit-count dst))))
                                   (t
                                    (prog1 (list :obs (env-add env obs-size :fixnum) w csr-syms)
                                      (push (list (unit-name dst) port obs-size (unit-count dst)) obs-blocks)
                                      (incf obs-size (unit-count dst))))))
                       (unit-inputs dst))))
      ;; входы без связей — значения по умолчанию
      (dolist (u units)
        (dolist (in (worm.dsl::component-inputs (worm.dsl::instance-component (worm.dsl::running-instance (unit-running u)))))
          (destructuring-bind (name tag default) in
            (declare (ignore tag))
            (unless (assoc name (unit-inputs u))
              (push (cons name (list :default (or default 0d0))) (unit-inputs u))))))
      ;; состояния нервно-мышечной части
      (dolist (u units)
        (unless (unit-mech-p u)
          (loop for (var . nil) in (worm.dsl::running-states (unit-running u))
                do (push (cons var (env-add env state-offset :fixnum)) (unit-states u))
                   (push (list (unit-name u) var state-offset (unit-count u)) neural-states)
                   (incf state-offset (unit-count u))))
        (setf (unit-states u) (nreverse (unit-states u)))
        (when (and (unit-mech-p u) (worm.dsl::running-states (unit-running u)))
          (error 'codegen-error :message (format nil "у экземпляра механики ~(~A~) есть состояние: механика — только координаты узлов" (unit-name u))))
        (when (worm.dsl::running-discrete (unit-running u))
          (when (unit-mech-p u)
            (error 'codegen-error :message (format nil "у экземпляра механики ~(~A~) есть дискретное состояние" (unit-name u))))
          (loop for (var . init) in (worm.dsl::running-discrete (unit-running u))
                do (push (cons var (env-add env discrete-offset :fixnum)) (unit-discretes u))
                   (push (list (unit-name u) var discrete-offset (unit-count u) init) discrete-states)
                   (incf discrete-offset (unit-count u))))))
    (values units param-names
            (make-model-layout
             :param-names (coerce param-names 'vector)
             :param-defaults (map 'dvec (lambda (n) (float (worm.params:param-value set n) 0d0)) param-names)
             :neural-states (nreverse neural-states)
             :discrete-states (mapcar #'butlast (reverse discrete-states))
             :z0 (let ((z (make-dvec discrete-offset)))
                   (loop for (nil nil off count init) in discrete-states
                         do (replace z init :start1 off :end1 (+ off count)))
                   z)
             :obs-blocks (nreverse obs-blocks) :syn-blocks (nreverse syn-blocks))
            obs-size syn-size state-offset)))

;;; Чтения переменных

(defun leaf-former (u param-names &key (pos-vector 'y) (aux-vector 'aux) (state-vector 'yn))
  (lambda (key)
    (ecase (first key)
      (:pos `(aref ,pos-vector (+ (* 3 ,(slot-node-symbol (second key) u)) ,(third key))))
      (:aux `(aref ,aux-vector (+ (* 3 ,(slot-node-symbol (second key) u)) ,(third key))))
      (:param `(aref p ,(or (position (second key) param-names)
                            (error 'codegen-error :message (format nil "нет параметра ~(~A~)" (second key))))))
      ((:array :element) `(aref ,(cdr (assoc key (unit-syms u) :test #'equal)) e))
      (:state `(aref ,state-vector (+ ,(cdr (assoc (second key) (unit-states u))) e)))
      (:input (let ((src (cdr (assoc (second key) (unit-inputs u)))))
                (ecase (first src)
                  (:default (float (second src) 0d0))
                  (:buffer `(aref ,(second src) e))
                  (:obs `(aref obs (+ ,(second src) e)))
                  (:syn `(aref syn (+ ,(second src) e))))))
      (:time 'time)
      (:vel (error 'codegen-error :message "скорость узлов в силе: она входит только в :damping"))
      (:discrete `(aref z (+ ,(or (cdr (assoc (second key) (unit-discretes u)))
                                  (error 'codegen-error :message (format nil "нет дискретной переменной ~(~A~)" (second key))))
                             e))))))

(defun slot-index (u slot)
  (if (integerp slot) slot (position slot (worm.dsl::family-slots (unit-family u) 0))))

(defun slot-node-symbol (slot u)
  (declare (ignore u))
  (intern (format nil "N~A" (if (integerp slot) slot (string slot))) :worm.codegen))

(defun used-slots (roots)
  (let ((slots '()))
    (dolist (n (reachable-nodes roots))
      (when (worm.ir:var-p n)
        (let ((k (worm.ir:var-key n)))
          (when (member (first k) '(:pos :aux :vel)) (pushnew (second k) slots :test #'equal)))))
    slots))

(defun element-loop (u roots param-names writer &key (geometry nil) extra-slots scratch force-hessian)
  "DOTIMES по элементам единицы U: корни ROOTS вычисляются, WRITER получает
их формы и возвращает формы записи. GEOMETRY — (окружение): быстрая
геометрия отсека считается, если корни используют примитивы объёма;
SCRATCH и FORCE-HESSIAN — готовый рабочий массив и обязательный гессиан
(структурный якобиан градиентной силы)."
  (let* ((kernel (kernel-of u))
         (slots (union (used-slots roots) extra-slots :test #'equal))
         (uses-volume (or force-hessian
                          (and geometry
                               (worm.ir:with-graph ((worm.dsl::kernel-graph kernel))
                                 (uses-primitive-p roots 'worm.dsl::volume 'worm.dsl::volume-grad 'worm.dsl::volume-hess)))))
         (scratch (or scratch
                      (when uses-volume
                        (env-add (car geometry) (worm.topology:make-volume-scratch (worm.dsl::kernel-ring-nodes kernel))
                                 :scratch))))
         (hessian (or force-hessian (and uses-volume (uses-primitive-p roots 'worm.dsl::volume-hess)))))
    (when uses-volume
      (check-volume-arguments roots (worm.dsl::kernel-ring-nodes kernel)))
    (worm.ir:with-graph ((worm.dsl::kernel-graph kernel))
      (multiple-value-bind (bindings forms)
          (emit-nodes roots (leaf-former u param-names) (builtin-prim-form scratch))
        `(dotimes (e ,(unit-count-sym u))
           (let* ((b (* e ,(unit-nslots u)))
                  ,@(loop for s in slots
                          collect `(,(slot-node-symbol s u)
                                    (the small-index (aref ,(unit-slots-sym u) (+ b ,(slot-index u s)))))))
             (declare (type small-index ,@(mapcar (lambda (s) (slot-node-symbol s u)) slots))
                      (type (unsigned-byte 40) b)
                      (ignorable b ,@(mapcar (lambda (s) (slot-node-symbol s u)) slots)))
             ,@(when uses-volume
                 `((worm.topology:compartment-geometry! y ,(unit-slots-sym u) b
                                                        ,(worm.dsl::kernel-ring-nodes kernel) ,scratch ,hessian)))
             ;; плоский LET и SETQ, а не LET*: LET* из тысяч связок SBCL
             ;; разворачивает во вложенные лямбды и переполняет стек
             (let ,(loop for (sym) in bindings collect (list sym 0d0))
               (declare (type double-float ,@(mapcar #'first bindings)))
               (setq ,@(loop for (sym expr) in bindings append (list sym expr)))
               ,@(funcall writer forms))))))))

(defun aggregation (u port target)
  "Свести вход PORT единицы U по отношению связи; TARGET — (буфер смещение)."
  (destructuring-bind (kind sym w csr-syms) (cdr (assoc port (unit-inputs u)))
    (declare (ignore kind sym))
    (destructuring-bind (off srcs ws) csr-syms
      (let ((src-buffer (cdr (assoc (worm.dsl::wire-src-port w)
                                    (unit-outputs (find (worm.dsl::wire-src-instance w) *units* :key #'unit-name))))))
        `(dotimes (e ,(unit-count-sym u))
           (let ((s 0d0))
             (declare (type double-float s))
             (loop for q of-type fixnum from (aref ,off e) below (aref ,off (1+ e))
                   do (incf s (* (aref ,ws q) (aref ,src-buffer (aref ,srcs q)))))
             (setf (aref ,(first target) (+ ,(second target) e)) s)))))))


(defun wired-ports (u to)
  "Выходы U со связями к механике (TO = :mech), к нервно-мышечной части
(:neural) или к любой (:any)."
  (let ((ports '()))
    (loop for (w) in *relations*
          when (eq (worm.dsl::wire-src-instance w) (unit-name u))
            do (let ((dst (find (worm.dsl::wire-dst-instance w) *units* :key #'unit-name)))
                 (when (or (eq to :any) (eq (and (unit-mech-p dst) t) (eq to :mech)))
                   (pushnew (worm.dsl::wire-src-port w) ports))))
    ports))

(defun internal-aggregations (u)
  "Сведение входов U из буферов своей подсистемы."
  (loop for (port . src) in (unit-inputs u)
        when (eq (first src) :buffer)
          collect (aggregation u port (list (second src) 0))))

(defun output-roots (u ports)
  (loop for (port . node) in (worm.dsl::kernel-outputs (kernel-of u))
        when (member port ports) collect (cons port node)))

(defun write-outputs (u pairs forms)
  (loop for (port) in pairs for f in forms
        collect `(setf (aref ,(cdr (assoc port (unit-outputs u))) e) ,f)))

;;; Функции модели

(defun observables-body (mech neural param-names)
  "Выходы механики, нужные нервно-мышечной части, и их сведение в OBS."
  (append
   (loop for u in mech
         for pairs = (output-roots u (wired-ports u :any))
         when pairs
           append (append (internal-aggregations u)
                          (list (element-loop u (mapcar #'cdr pairs) param-names
                                              (lambda (forms) (write-outputs u pairs forms))))))
   (loop for u in neural
         append (loop for (port . src) in (unit-inputs u)
                      when (eq (first src) :obs)
                        collect (aggregation u port (list 'obs (second src)))))))

(defun neural-pass (neural param-names derivs-writer &key (ports-fn (lambda (u) (wired-ports u :neural))))
  "Проход по нервно-мышечной части: сведение входов, выходы в буферы и,
если задан DERIVS-WRITER, производные."
  (loop for u in neural
        for pairs = (output-roots u (funcall ports-fn u))
        for derivs = (and derivs-writer (worm.dsl::kernel-derivs (kernel-of u)))
        when (or pairs derivs)
          append (append (internal-aggregations u)
                         (list (element-loop u (append (mapcar #'cdr pairs) (mapcar #'cdr derivs)) param-names
                                             (lambda (forms)
                                               (append (write-outputs u pairs (subseq forms 0 (length pairs)))
                                                       (when derivs
                                                         (funcall derivs-writer u derivs (nthcdr (length pairs) forms))))))))))

(defun neural-rhs-body (neural param-names)
  (neural-pass neural param-names
               (lambda (u derivs forms)
                 (loop for (var) in derivs for f in forms
                       collect `(setf (aref f (+ ,(cdr (assoc var (unit-states u))) e)) ,f)))))

(defun neural-jacobian-body (neural param-names)
  (append
   (neural-pass neural param-names nil)
   (coupled-jacobian-body neural param-names)
   (loop for u in neural
         for kernel = (kernel-of u)
         for entries = (worm.ir:with-graph ((worm.dsl::kernel-graph kernel))
                         (loop for (var . node) in (worm.dsl::kernel-derivs kernel)
                               append (loop for (wrt) in (unit-states u)
                                            for d = (worm.ir:derivative node (worm.ir:var (list :state wrt)))
                                            unless (worm.ir:zero-p d) collect (list var wrt d))))
         when entries
           append (append (internal-aggregations u)
                          (list (element-loop u (mapcar #'third entries) param-names
                                              (lambda (forms)
                                                (loop for (var wrt) in entries for f in forms
                                                      collect `(incf (aref jac (+ ,(cdr (assoc var (unit-states u))) e)
                                                                           (+ ,(cdr (assoc wrt (unit-states u))) e))
                                                                     ,f)))))))))

;;; Якобиан через связи: вход PORT единицы U сведён из выхода источника,
;;; который зависит от состояния источника. По цепному правилу
;;;   ∂f_u(e)/∂x_src(s) = ∂f_u/∂in(e) · w(e, s) · ∂out_src/∂x_src(s).
;;; Производные выходов источников считаются поэлементно в буферы, затем
;;; для каждого элемента приёмника обходятся связи его строки CSR.

(defun state-dependent-input-p (u port)
  "Вход PORT единицы U идёт из буфера выхода, зависящего от состояния источника."
  (let ((src (cdr (assoc port (unit-inputs u)))))
    (when (eq (first src) :buffer)
      (let* ((w (third src))
             (src-unit (find (worm.dsl::wire-src-instance w) *units* :key #'unit-name))
             (node (cdr (assoc (worm.dsl::wire-src-port w) (worm.dsl::kernel-outputs (kernel-of src-unit))))))
        (worm.ir:with-graph ((worm.dsl::kernel-graph (kernel-of src-unit)))
          (some (lambda (k) (eq (first k) :state))
                (mapcar #'worm.ir:var-key (remove-if-not #'worm.ir:var-p (reachable-nodes (list node))))))))))

(defun coupled-jacobian-body (neural param-names)
  (let ((output-derivs (make-hash-table :test 'equal)) ; (источник порт состояние) → буфер
        (forms '()))
    ;; производные выходов источников по их состояниям
    (dolist (u neural)
      (loop for (port . src) in (unit-inputs u)
            when (state-dependent-input-p u port)
              do (let* ((w (third src))
                        (su (find (worm.dsl::wire-src-instance w) *units* :key #'unit-name))
                        (sport (worm.dsl::wire-src-port w))
                        (kernel (kernel-of su))
                        (node (cdr (assoc sport (worm.dsl::kernel-outputs kernel))))
                        (entries (worm.ir:with-graph ((worm.dsl::kernel-graph kernel))
                                   (loop for (wrt) in (unit-states su)
                                         for d = (worm.ir:derivative node (worm.ir:var (list :state wrt)))
                                         unless (or (worm.ir:zero-p d) (gethash (list (unit-name su) sport wrt) output-derivs))
                                           collect (list wrt d)))))
                   (when entries
                     (dolist (en entries)
                       (setf (gethash (list (unit-name su) sport (first en)) output-derivs)
                             (env-add *env* (make-dvec (unit-count su)) :dvec)))
                     (push (append (internal-aggregations su)
                                   (list (element-loop su (mapcar #'second entries) param-names
                                                       (lambda (fs)
                                                         (loop for (wrt) in entries for f in fs
                                                               collect `(setf (aref ,(gethash (list (unit-name su) sport wrt) output-derivs) e) ,f))))))
                           forms)))))
    (setf forms (apply #'append (nreverse forms)))
    ;; производные приёмников по входам и сборка по связям
    (append
     forms
     (loop for u in neural
           for kernel = (kernel-of u)
           for ports = (loop for (port) in (unit-inputs u) when (state-dependent-input-p u port) collect port)
           for entries = (when ports
                           (worm.ir:with-graph ((worm.dsl::kernel-graph kernel))
                             (loop for (var . node) in (worm.dsl::kernel-derivs kernel)
                                   append (loop for port in ports
                                                for d = (worm.ir:derivative node (worm.ir:var (list :input port)))
                                                unless (worm.ir:zero-p d) collect (list var port d)))))
           when entries
             append (append
                     (internal-aggregations u)
                     (list (element-loop
                            u (mapcar #'third entries) param-names
                            (lambda (fs)
                              (loop for (var port) in entries for f in fs
                                    append (destructuring-bind (kind sym w (off srcs ws)) (cdr (assoc port (unit-inputs u)))
                                             (declare (ignore kind sym))
                                             (let* ((su (find (worm.dsl::wire-src-instance w) *units* :key #'unit-name))
                                                    (sport (worm.dsl::wire-src-port w))
                                                    (row `(+ ,(cdr (assoc var (unit-states u))) e)))
                                               (loop for (wrt . off-sym) in (unit-states su)
                                                     for buf = (gethash (list (unit-name su) sport wrt) output-derivs)
                                                     when buf
                                                       collect `(let ((dfdin ,f))
                                                                  (declare (type double-float dfdin))
                                                                  (loop for q of-type fixnum from (aref ,off e) below (aref ,off (1+ e))
                                                                        for s of-type fixnum = (aref ,srcs q)
                                                                        do (incf (aref jac ,row (+ ,off-sym s))
                                                                                 (* dfdin (aref ,ws q) (aref ,buf s)))))))))))))))))

(defun neural-discrete-body (neural param-names)
  "События на границе шага: условия и значения всех событий элемента
вычисляются по z до применения, затем применяются по порядку."
  (append
   (neural-pass neural param-names nil)
   (loop for u in neural
         for events = (worm.dsl::kernel-events (kernel-of u))
         when events
           append (append (internal-aggregations u)
                          (list (element-loop u (loop for (nil nil c v) in events append (list c v)) param-names
                                              (lambda (forms)
                                                (loop for (direction var) in events
                                                      for (c v) on forms by #'cddr
                                                      collect `(when (,(if (eq direction :rising) '> '<) ,c 0d0)
                                                                 (setf (aref z (+ ,(cdr (assoc var (unit-discretes u))) e))
                                                                       ,(if (numberp v) (float v 0d0) v)))))))))))

(defun neural-output-body (neural param-names)
  (append
   (neural-pass neural param-names nil :ports-fn (lambda (u) (wired-ports u :any)))
   (loop for u in *units*
         when (unit-mech-p u)
           append (loop for (port . src) in (unit-inputs u)
                        when (eq (first src) :syn)
                          collect (aggregation u port (list 'syn (second src)))))))

(defun activation-unit (neural)
  "Экземпляр на мышцах с выходом ACTIVE — его активации идут в кадры."
  (find-if (lambda (u) (and (eq (unit-family u) :muscle)
                            (assoc :active (worm.dsl::kernel-outputs (kernel-of u)))))
           neural))

(defun activation-body (neural param-names)
  "Выход ACTIVE мышц в OUT: сначала выходы, которые нервно-мышечная часть
читает сама (привод генератора), затем входы и выход мышц."
  (let ((u (activation-unit neural)))
    (append
     (neural-pass neural param-names nil)
     (internal-aggregations u)
     (list (element-loop u (list (cdr (assoc :active (worm.dsl::kernel-outputs (kernel-of u))))) param-names
                         (lambda (forms) `((setf (aref out e) ,(first forms)))))))))

(defun mech-forces-body (mech param-names)
  (loop for u in mech
        for kernel = (kernel-of u)
        for pairs = (output-roots u (wired-ports u :mech))
        for forces = (worm.dsl::kernel-forces kernel)
        when (or pairs forces)
          append (append (internal-aggregations u)
                         (list (element-loop u (append (mapcar #'cdr pairs) (mapcar #'third forces)) param-names
                                             (lambda (forms)
                                               (append (write-outputs u pairs (subseq forms 0 (length pairs)))
                                                       (loop for (slot c) in forces for f in (nthcdr (length pairs) forms)
                                                             collect `(incf (aref g (+ (* 3 ,(slot-node-symbol slot u)) ,c)) ,f))))
                                             :geometry (list *env*))))))


(defun structured-gradients (u)
  "Силы k·∂V/∂x из :gradient объёма отсека: их якобиан k·∇²V + ∇V ⊗ ∇k
собирается циклом по гессиану быстрой геометрии, а не поэлементно."
  (let ((kernel (kernel-of u)))
    (worm.ir:with-graph ((worm.dsl::kernel-graph kernel))
      (loop for (s k indices) in (worm.dsl::kernel-gradients kernel)
            when (and (eq (worm.ir:node-op s) :prim) (eq (car (worm.ir:node-data s)) 'worm.dsl::volume)
                      (= (length indices) (* 6 (worm.dsl::kernel-ring-nodes kernel))))
              collect (list s k indices)))))

(defun jacobian-entries (u)
  "Записи локального якобиана: ((строка-слот c столбец-слот d узел) …)."
  (let ((kernel (kernel-of u)))
    (worm.ir:with-graph ((worm.dsl::kernel-graph kernel))
      (loop for (slot c node) in (worm.dsl::kernel-forces kernel)
            for index from 0
            unless (some (lambda (g) (member index (third g))) (structured-gradients u))
            append (let ((leaves (remove-if-not (lambda (k) (eq (first k) :pos))
                                                (mapcar #'worm.ir:var-key
                                                        (remove-if-not #'worm.ir:var-p (reachable-nodes (list node)))))))
                     (dolist (k (mapcar #'worm.ir:var-key (remove-if-not #'worm.ir:var-p (reachable-nodes (list node)))))
                       (when (and (eq (first k) :input)
                                  (eq (first (cdr (assoc (second k) (unit-inputs u)))) :buffer))
                         (error 'codegen-error :message (format nil "сила ~(~A~) зависит от входа из механики: цепочка якобиана через связи механики — не реализована" (unit-name u)))))
                     (loop for (nil s d) in (sort (copy-list leaves) #'string< :key (lambda (k) (format nil "~S" k)))
                           for dv = (worm.ir:derivative node (worm.ir:var (list :pos s d)))
                           unless (worm.ir:zero-p dv) collect (list slot c s d dv)))))))

(defun damping-entries (u)
  "Члены вязкости: ((коэффициент ((слот c) . g) …) …)."
  (let ((kernel (kernel-of u)))
    (worm.ir:with-graph ((worm.dsl::kernel-graph kernel))
      (loop for (coef . rate) in (worm.dsl::kernel-damping kernel)
            collect (list coef
                          (loop for v in (worm.dsl::velocity-leaves rate)
                                collect (cons (rest (worm.ir:var-key v)) (worm.ir:derivative rate v))))))))

(defvar *damping-geometry* t
  "Добавлять ли в матрицу шага производную вязких сил по положению при
скорости прошлого шага (решение 26). NIL — как у ручных ядер этапов 3 и 5.")

(defun damping-geometry-entries (u)
  "Поправка матрицы шага от поворота вязких сил: C = ∂(B(y)v)/∂y при v = aux.
Член вязкости c·r даёт силу c·r·∂r/∂v на каждую скорость; r линейна по
скоростям, r = Σ g_k·v_k, поэтому при скоростях прошлого шага
f_i = c·(Σ g_k·aux_k)·g_i, и C_ij = ∂f_i/∂y_j. Записи — как у якобиана:
((строка-слот c столбец-слот d узел) …)."
  (when *damping-geometry*
    (let ((kernel (kernel-of u)))
      (worm.ir:with-graph ((worm.dsl::kernel-graph kernel))
        (loop for (coef grads) in (damping-entries u)
              for r-aux = (apply #'worm.ir:ir+ (loop for ((slot c) . g) in grads
                                                     collect (worm.ir:ir* g (worm.ir:var (list :aux slot c)))))
              append (loop for ((slot c) . g) in grads
                           for f = (worm.ir:ir* coef r-aux g)
                           append (let ((leaves (remove-duplicates
                                                 (remove-if-not (lambda (k) (eq (first k) :pos))
                                                                (mapcar #'worm.ir:var-key
                                                                        (remove-if-not #'worm.ir:var-p (reachable-nodes (list f)))))
                                                 :test #'equal)))
                                    (loop for (nil s d) in (sort leaves #'string< :key (lambda (k) (format nil "~S" k)))
                                          for dv = (worm.ir:derivative f (worm.ir:var (list :pos s d)))
                                          unless (worm.ir:zero-p dv) collect (list slot c s d dv)))))))))

(defun scatter-map (u entries index-fn)
  "Карта индексов: для элемента e и записи m — INDEX-FN строки и столбца."
  (let* ((slots (worm.dsl::running-slots (unit-running u)))
         (m (length entries))
         (map (make-array (* (unit-count u) m) :element-type 'fixnum)))
    (dotimes (e (unit-count u) map)
      (let ((nodes (aref slots e)))
        (loop for entry in entries for k from 0
              do (setf (aref map (+ (* e m) k))
                       (funcall index-fn nodes entry)))))))

(defun global-index (u nodes slot c) (+ (* 3 (aref nodes (slot-index u slot))) c))

(defun mech-matrix-plan (mech)
  "Записи якобиана и вязкости всех единиц механики и фактическая ширина ленты."
  (let ((kl 0) (ku 0) (plans '()))
    (dolist (u mech)
      (let ((jac (jacobian-entries u)) (damp (damping-entries u)) (geo (damping-geometry-entries u))
            (slots (worm.dsl::running-slots (unit-running u))))
        (when slots
          (dotimes (e (unit-count u))
            (let ((nodes (aref slots e)))
              (dolist (j (append jac geo))
                (destructuring-bind (rs c ss d node) j
                  (declare (ignore node))
                  (let ((row (global-index u nodes rs c)) (col (global-index u nodes ss d)))
                    (setf kl (max kl (- row col)) ku (max ku (- col row))))))
              (dolist (term damp)
                (dolist (gi (second term))
                  (dolist (gj (second term))
                    (let ((row (global-index u nodes (first (car gi)) (second (car gi))))
                          (col (global-index u nodes (first (car gj)) (second (car gj)))))
                      (setf kl (max kl (- row col)) ku (max ku (- col row))))))))))
        (when (and slots (structured-gradients u))
          (let ((m (* 2 (worm.dsl::kernel-ring-nodes (kernel-of u)))))
            (dotimes (e (unit-count u))
              (let* ((nodes (aref slots e))
                     (lo (* 3 (loop for j below m minimize (aref nodes j))))
                     (hi (+ 2 (* 3 (loop for j below m maximize (aref nodes j))))))
                (setf kl (max kl (- hi lo)) ku (max ku (- hi lo)))))))
        (push (list u jac damp geo) plans)))
    (values (nreverse plans) kl ku)))

(defun entries-loop (u entries param-names kl ku factor)
  "Цикл по элементам: записи (строка-слот c столбец-слот d узел) в ленту с
множителем FACTOR (форма)."
  (let ((map (env-add *env* (scatter-map u entries (lambda (nodes entry)
                                                     (destructuring-bind (rs c ss d node) entry
                                                       (declare (ignore node))
                                                       (band-index (global-index u nodes rs c)
                                                                   (global-index u nodes ss d) kl ku))))
                      :ivec))
        (m (length entries)))
    (element-loop u (mapcar #'fifth entries) param-names
                  (lambda (forms)
                    (loop for f in forms for k from 0
                          collect `(incf (aref ab (aref ,map (+ (* e ,m) ,k))) (* ,factor ,f))))
                  :geometry (list *env*))))

(defun mech-matrix-body (plans param-names kl ku)
  (loop for (u jac damp geo) in plans
        when (or jac damp geo (structured-gradients u))
          append (append
                  (internal-aggregations u)
                  ;; h·C: поворот вязких сил при скоростях прошлого шага
                  (when geo (list (entries-loop u geo param-names kl ku '(- mh))))
                  (when jac
                    (let ((map (env-add *env* (scatter-map u jac (lambda (nodes entry)
                                                                   (destructuring-bind (rs c ss d node) entry
                                                                     (declare (ignore node))
                                                                     (band-index (global-index u nodes rs c)
                                                                                 (global-index u nodes ss d) kl ku))))
                                        :ivec))
                          (m (length jac)))
                      (list (element-loop u (mapcar #'fifth jac) param-names
                                          (lambda (forms)
                                            (loop for f in forms for k from 0
                                                  collect `(incf (aref ab (aref ,map (+ (* e ,m) ,k))) (* mh ,f))))
                                          :geometry (list *env*)))))
                  (loop for (nil k nil) in (structured-gradients u)
                        collect (structured-gradient-loop u k param-names kl ku))
                  (loop for (coef grads) in damp
                        for pairs = (loop for gi in grads append (loop for gj in grads collect (cons gi gj)))
                        for map = (env-add *env* (scatter-map u pairs
                                                              (lambda (nodes pair)
                                                                (band-index (global-index u nodes (first (car (car pair))) (second (car (car pair))))
                                                                            (global-index u nodes (first (car (cdr pair))) (second (car (cdr pair))))
                                                                            kl ku)))
                                           :ivec)
                        for m = (length pairs)
                        collect (element-loop u (cons coef (mapcar #'cdr grads)) param-names
                                              (lambda (forms)
                                                (let ((c (first forms)) (gs (rest forms)))
                                                  (loop for (gi . gj) in pairs for k from 0
                                                        collect `(incf (aref ab (aref ,map (+ (* e ,m) ,k)))
                                                                       (* ,c ,(nth (position gi grads) gs)
                                                                          ,(nth (position gj grads) gs)))))))))))

(defun structured-gradient-loop (u k param-names kl ku)
  "−h·(k·∇²V + ∇V ⊗ ∇k) отсека: производные k по 6K координатам — узлы IR,
гессиан и градиент — из быстрой геометрии, сборка — вложенным циклом."
  (let* ((kernel (kernel-of u))
         (kk (worm.dsl::kernel-ring-nodes kernel))
         (m (* 6 kk))
         (dk (worm.ir:with-graph ((worm.dsl::kernel-graph kernel))
               (loop for j below m
                     collect (worm.ir:derivative k (worm.ir:var (list :pos (floor j 3) (mod j 3)))))))
         (dk-buffer (env-add *env* (make-dvec m) :dvec))
         (scratch (env-add *env* (worm.topology:make-volume-scratch kk) :scratch))
         (slots (worm.dsl::running-slots (unit-running u)))
         (map (let ((v (make-array (* (unit-count u) m m) :element-type 'fixnum)))
                (dotimes (e (unit-count u) v)
                  (let ((nodes (aref slots e)))
                    (dotimes (j m)
                      (dotimes (q m)
                        (setf (aref v (+ (* e m m) (* j m) q))
                              (band-index (+ (* 3 (aref nodes (floor j 3))) (mod j 3))
                                          (+ (* 3 (aref nodes (floor q 3))) (mod q 3)) kl ku))))))))
         (map-sym (env-add *env* map :ivec)))
    (element-loop u (cons k dk) param-names
                  (lambda (forms)
                    (let ((kf (first forms)))
                      (append
                       (loop for f in (rest forms) for j from 0 collect `(setf (aref ,dk-buffer ,j) ,f))
                       `((let ((grad (worm.topology:volume-scratch-grad ,scratch))
                               (hess (worm.topology:volume-scratch-hess ,scratch))
                               (base (* e ,(* m m))))
                           (declare (type (simple-array double-float (*)) grad)
                                    (type (simple-array double-float (* *)) hess)
                                    (type (unsigned-byte 40) base))
                           (dotimes (j ,m)
                             (dotimes (q ,m)
                               (incf (aref ab (aref ,map-sym (+ base (* j ,m) q)))
                                     (* mh (+ (* ,kf (aref hess j q)) (* (aref grad j) (aref ,dk-buffer q))))))))))))
                  :scratch scratch :force-hessian t)))

(defun check-state-body (mech param-names)
  (loop for u in mech
        for checks = (worm.dsl::kernel-checks (kernel-of u))
        when checks
          collect (element-loop u (mapcar #'cdr checks) param-names
                                (lambda (forms)
                                  (loop for (cause) in checks for f in forms
                                        collect `(when (< ,f 0d0) (return-from check (values ,cause e)))))
                                :geometry (list *env*))))

;;; Компиляция

(defvar *cache-directory* nil "Каталог кэша кода; NIL — cache/ в корне проекта.")

(defun compile-form (form notes)
  "Скомпилировать FORM, собрав заметки оптимизатора в NOTES (ячейка списка)."
  (handler-bind ((sb-ext:compiler-note
                   (lambda (c)
                     (push (princ-to-string c) (car notes))
                     (muffle-warning c)))
                 (warning (lambda (c) (push (format nil "предупреждение: ~A" c) (car notes)) (muffle-warning c))))
    (let ((*error-output* (make-broadcast-stream)))
      (multiple-value-bind (fn warnings-p failure-p) (compile nil form)
        (declare (ignore warnings-p))
        (when failure-p
          (error 'codegen-error :message (format nil "сгенерированный код не скомпилировался:~{~%  ~A~}" (car notes))))
        fn))))

(defvar *factories* (make-hash-table :test 'equal) "Фабрики, загруженные из кэша: ключ → ((функция . фабрика) …).")
(defvar *factory-notes* (make-hash-table :test 'equal) "Заметки оптимизатора кэшированных фабрик.")

(defun cache-directory ()
  (or *cache-directory* (merge-pathnames "cache/codegen/" (project-root))))

(defun cache-key (spec ring-nodes medium)
  (worm.platform:md5-hex (format nil "~A ~D ~A ~D ~A ~A~:[ без поворота вязкости~;~]" (worm.dsl:model-spec-hash spec) ring-nodes medium
                                 +codegen-version+ (lisp-implementation-type) (lisp-implementation-version)
                                 *damping-geometry*)))

(defun compile-factories (forms key cache)
  "Фабрики функций модели: ((имя . фабрика) …) и заметки оптимизатора. При
CACHE текст пишется в cache/codegen/<ключ>.lisp и компилируется в fasl;
следующая сборка той же модели загружает fasl без компиляции."
  (if (not cache)
      (let ((notes (list '())))
        (values (loop for (name . form) in forms collect (cons name (compile-form form notes)))
                (reverse (car notes))))
      (let* ((dir (cache-directory))
             (source (merge-pathnames (format nil "~A.lisp" key) dir))
             (fasl (compile-file-pathname source)))
        (unless (gethash key *factories*)
          (unless (probe-file fasl)
            (ensure-directories-exist source)
            (with-open-file (out source :direction :output :if-exists :supersede :external-format :utf-8)
              (with-standard-io-syntax
                (let ((*package* (find-package :worm.codegen)) (*print-readably* t) (*print-circle* nil))
                  (format out ";;;; Сгенерированный код модели, кодогенератор ~D. Не править руками.~%" +codegen-version+)
                  (print '(in-package #:worm.codegen) out)
                  (print `(setf (gethash ,key *factories*)
                                (list ,@(loop for (name . form) in forms collect `(cons ,name ,form))))
                         out))))
            (let ((notes '()))
              (handler-bind ((sb-ext:compiler-note
                               (lambda (c) (push (princ-to-string c) notes) (muffle-warning c)))
                             (warning (lambda (c) (push (format nil "предупреждение: ~A" c) notes) (muffle-warning c))))
                (let ((*error-output* (make-broadcast-stream)) (*standard-output* (make-broadcast-stream)))
                  (multiple-value-bind (out warnings-p failure-p) (compile-file source :output-file fasl)
                    (declare (ignore out warnings-p))
                    (when failure-p
                      (delete-file fasl)
                      (error 'codegen-error :message (format nil "кэш ~A не скомпилировался:~{~%  ~A~}" source notes))))))
              (with-open-file (out (make-pathname :type "notes" :defaults source) :direction :output
                                                                                  :if-exists :supersede :external-format :utf-8)
                (with-standard-io-syntax (prin1 (reverse notes) out)))))
          (load fasl)
          (let ((notes-file (make-pathname :type "notes" :defaults source)))
            (setf (gethash key *factory-notes*)
                  (and (probe-file notes-file)
                       (with-open-file (in notes-file :external-format :utf-8)
                         (with-standard-io-syntax (let ((*read-eval* nil)) (read in nil '()))))))))
        (values (gethash key *factories*) (gethash key *factory-notes*)))))

(defun place-on-surface (coords kk rings)
  (let ((out (copy-seq coords)))
    (dotimes (i rings out)
      (let ((lowest (loop for k below kk minimize (aref coords (+ (* 3 (+ (* i kk) k)) 2)))))
        (dotimes (k kk) (decf (aref out (+ (* 3 (+ (* i kk) k)) 2)) lowest))))))

(defun compile-model (spec &key (params "water-v1") medium rings ring-nodes element-arrays cache
                                (damping-geometry t))
  "compiled-model по SPEC (model-spec или имя модели). Остальные ключи — как
у WORM.DSL:INSTANTIATE; CACHE — брать скомпилированный код из кэша fasl.
Раскладка, начальное состояние, параметры и заметки оптимизатора — в
COMPILED-MODEL-LAYOUT."
  (let* ((spec (if (worm.dsl::model-spec-p spec) spec (worm.dsl:find-model spec)))
         (set (if (worm.params:param-set-p params) params (worm.params:load-param-set params)))
         (model (worm.dsl:instantiate spec :params set :medium medium :rings rings :ring-nodes ring-nodes
                                           :element-arrays element-arrays))
         (topology (worm.dsl::model-instance-topology model))
         (kk (worm.topology:tube-ring-nodes topology))
         (n-mech (worm.dsl::model-instance-n-mech model))
         (*env* (make-env-plan))
         (*damping-geometry* damping-geometry)
         (notes (list '())))
    (multiple-value-bind (units param-names layout obs-size syn-size n-neural) (plan-model model *env* set)
      (let* ((*units* units)
             (*relations* (worm.dsl::model-instance-relations model))
             (mech (remove-if-not #'unit-mech-p units))
             (neural (remove-if #'unit-mech-p units))
             (aux-p (or (some (lambda (u) (some (lambda (k) (eq (first k) :aux))
                                                (worm.dsl::kernel-leaves (kernel-of u))))
                              units)
                        (and *damping-geometry*
                             (some (lambda (u) (and (unit-mech-p u) (worm.dsl::kernel-damping (kernel-of u)))) units)))))
        (multiple-value-bind (plans kl ku) (mech-matrix-plan mech)
          (let* ((forms
                   (list
                    (cons :observables
                          (factory-form *env* '(y aux p obs) '((type dvec y aux p obs) (ignorable y aux p obs))
                                        `((let ((time 0d0)) (declare (ignorable time))
                                            ,@(observables-body mech neural param-names))
                                          (values))))
                    (cons :neural-discrete
                          (factory-form *env* '(yn obs z p clock) '((type dvec yn obs z p clock) (ignorable yn obs z p clock))
                                        `((let ((time (aref clock ,+clock-time+))) (declare (ignorable time))
                                            ,@(neural-discrete-body neural param-names))
                                          (values))))
                    (cons :neural-rhs
                          (factory-form *env* '(yn obs z p clock f) '((type dvec yn obs z p clock f) (ignorable yn obs z p clock f))
                                        `((let ((time (aref clock ,+clock-time+))) (declare (ignorable time))
                                            ,@(neural-rhs-body neural param-names))
                                          (values))))
                    (cons :neural-jacobian
                          (factory-form *env* '(yn obs z p clock jac)
                                        '((type dvec yn obs z p clock) (type (simple-array double-float (* *)) jac)
                                          (ignorable yn obs z p clock jac))
                                        `((let ((time (aref clock ,+clock-time+))) (declare (ignorable time))
                                            ,@(neural-jacobian-body neural param-names))
                                          (values))))
                    (cons :neural-output
                          (factory-form *env* '(yn obs z p clock syn) '((type dvec yn obs z p clock syn) (ignorable yn obs z p clock syn))
                                        `((let ((time (+ (aref clock ,+clock-time+) (aref clock ,+clock-h+))))
                                            (declare (ignorable time))
                                            ,@(neural-output-body neural param-names))
                                          (values))))
                    (cons :mech-forces
                          (factory-form *env* '(y syn p g) '((type dvec y syn p g) (ignorable y syn p g))
                                        `((let ((time 0d0) (aux y)) (declare (ignorable time aux))
                                            ,@(mech-forces-body mech param-names))
                                          (values))))
                    (cons :mech-matrix
                          (factory-form *env* '(y aux syn p clock ab) '((type dvec y aux syn p clock ab) (ignorable y aux syn p clock ab))
                                        `((let ((time 0d0) (mh (- (aref clock ,+clock-h+))))
                                            (declare (ignorable time) (type double-float mh))
                                            ,@(mech-matrix-body plans param-names kl ku))
                                          (values))))
                    (cons :check-state
                          (factory-form *env* '(y p) '((type dvec y p) (ignorable y p))
                                        `((block check
                                            (let ((time 0d0) (aux y)) (declare (ignorable time aux))
                                              ,@(check-state-body mech param-names))
                                            nil))))))
                 (forms (if (activation-unit neural)
                             (append forms
                                     (list (cons :activation
                                                 (factory-form *env* '(yn obs z p clock out)
                                                               '((type dvec yn obs z p clock out) (ignorable yn obs z p clock out))
                                                               `((let ((time (aref clock ,+clock-time+))) (declare (ignorable time))
                                                                   ,@(activation-body neural param-names))
                                                                 (values))))))
                             forms))
                 (env (env-vector *env*))
                 (functions (multiple-value-bind (factories factory-notes)
                                (compile-factories forms (cache-key spec kk (worm.dsl::model-instance-medium model)) cache)
                              (setf (car notes) (reverse factory-notes))
                              (loop for (name . factory) in factories
                                    collect (cons name (funcall factory env)))))
                 (rings (worm.topology:tube-rings topology))
                 (y0 (let ((rest (worm.topology:tube-rest-coords topology)))
                       ;; на агаре тело лежит на боку на плоскости z = 0
                       (if (eq (worm.dsl::model-instance-medium model) :agar)
                           (place-on-surface rest kk rings)
                           rest)))
                 (yn0 (let ((v (make-dvec n-neural)))
                        (loop for (inst var off count) in (model-layout-neural-states layout)
                              do (replace v (cdr (assoc var (worm.dsl::running-states
                                                             (find inst (worm.dsl::model-instance-running model)
                                                                   :key #'running-name))))
                                          :start1 off))
                        v)))
            (setf (model-layout-instance layout) model
                  (model-layout-units layout) units
                  (model-layout-y0 layout) y0 (model-layout-yn0 layout) yn0
                  (model-layout-kl layout) kl (model-layout-ku layout) ku
                  (model-layout-notes layout) (reverse (car notes))
                  (model-layout-forms layout) forms
                  (model-layout-medium layout) (worm.dsl::model-instance-medium model)
                  (model-layout-activation layout) (cdr (assoc :activation functions)))
            (flet ((fn (name) (cdr (assoc name functions))))
              (make-compiled-model
               :name (worm.dsl:model-spec-name spec) :hash (worm.dsl:model-spec-hash spec) :layout layout
               :n-mech n-mech :n-neural n-neural
               :n-discrete (length (model-layout-z0 layout)) :n-observables obs-size :n-syn syn-size
               :n-aux (if aux-p n-mech 0) :lower-band kl :upper-band ku
               :topology topology :anatomy (worm.dsl::model-instance-anatomy model)
               :observables (and (plusp obs-size) (fn :observables))
               :neural-discrete (and (some (lambda (u) (worm.dsl::kernel-events (kernel-of u))) neural)
                                     (fn :neural-discrete))
               :neural-rhs (and (plusp n-neural) (fn :neural-rhs))
               :neural-jacobian (and (plusp n-neural) (fn :neural-jacobian))
               :neural-output (and (plusp syn-size) (fn :neural-output))
               :mech-forces (fn :mech-forces) :mech-matrix (fn :mech-matrix)
               :check-state (fn :check-state)))))))))

(defun model-param-vector (compiled &optional params)
  "Вектор p для COMPILED: значения из набора PARAMS (по умолчанию — тот,
с которым модель собрана)."
  (let ((layout (compiled-model-layout compiled)))
    (if params
        (let ((set (if (worm.params:param-set-p params) params (worm.params:load-param-set params))))
          (map 'dvec (lambda (n) (float (worm.params:param-value set n) 0d0)) (model-layout-param-names layout)))
        (copy-seq (model-layout-param-defaults layout)))))

(defun model-activations (compiled y-neural obs params clock out)
  "Активации мышц 0…1 в OUT (double-float) по состоянию нервно-мышечной
части; NIL, если в модели нет мышц с выходом ACTIVE."
  (let ((fn (model-layout-activation (compiled-model-layout compiled))))
    (when fn
      (fill out 0d0)
      (funcall fn y-neural obs (make-dvec 0) params clock out)
      t)))

(defun model-param-names (compiled)
  "Имена параметров вектора p по порядку (ключевые слова)."
  (model-layout-param-names (compiled-model-layout compiled)))

(defun model-initial-state (compiled)
  "Начальные y-mech, y-neural и z (копии)."
  (let ((layout (compiled-model-layout compiled)))
    (values (copy-seq (model-layout-y0 layout)) (copy-seq (model-layout-yn0 layout))
            (copy-seq (model-layout-z0 layout)))))

(defun model-discrete-states (compiled)
  "Раскладка z: ((экземпляр переменная смещение число) …)."
  (model-layout-discrete-states (compiled-model-layout compiled)))

(defun model-medium (compiled) (model-layout-medium (compiled-model-layout compiled)))

(defun model-neural-states (compiled)
  "Раскладка y-neural: ((экземпляр переменная смещение число) …)."
  (model-layout-neural-states (compiled-model-layout compiled)))

(defun model-notes (compiled) (model-layout-notes (compiled-model-layout compiled)))
(defun model-forms (compiled) (model-layout-forms (compiled-model-layout compiled)))
(defun model-instance-of (compiled) (model-layout-instance (compiled-model-layout compiled)))

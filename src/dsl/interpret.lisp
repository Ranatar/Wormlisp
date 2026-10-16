;;;; src/dsl/interpret.lisp — эталонный интерпретатор модели.
;;;;
;;;; Архитектура, «Эталонный интерпретатор»: модель исполняется прямо по
;;;; ядрам IR обобщённой арифметикой. Медленно, но это эталон для
;;;; кодогенерации этапа 7 и инструмент отладки одного ядра.
;;;;
;;;; INSTANTIATE собирает по model-spec всё, что нужно для исполнения:
;;;; топологию и анатомию, элементы семейств и их слоты, величины покоя,
;;;; поэлементные массивы, значения параметров, начальное состояние и
;;;; отношения связей. INTERPRET вычисляет при заданных координатах и
;;;; скоростях прошлого шага выходы экземпляров (в порядке связей), силы на
;;;; узлы и производные состояния; INTERPRET-DAMPING — матрицу вязкости D из
;;;; членов :damping: функция Рэлея ½·Σ коэф·скорость², D = Σ коэф·g gᵀ,
;;;; g = ∂скорость/∂v.

(in-package #:worm.dsl)

(defstruct (running (:copier nil))
  instance kernel count
  slots          ; simple-vector: слоты элемента → номера узлов (вектор на элемент) или NIL
  elements       ; ((имя . dvec) …) — величины элемента (:element имя)
  arrays         ; ((имя . dvec) …) — поэлементные параметры
  params         ; ((имя . число) …)
  states         ; ((имя . dvec) …)
  discrete       ; ((имя . dvec) …)
  inputs         ; ((имя . dvec) …) — значения входов на шаге
  outputs        ; ((имя . dvec) …)
  derivs         ; ((имя . dvec) …)
  evaluator      ; корни: выходы, силы, производные
  damping-evaluator damping-roots)

(defstruct (model-instance (:copier nil))
  spec topology anatomy binding medium time n-mech
  running        ; экземпляры: механика в порядке связей, затем нервно-мышечная часть
  mechanical     ; имена экземпляров механики
  relations)     ; ((провод . csr) …), csr — вектор на элемент-получатель: ((источник . вес) …)

(defun family-count (family topology binding)
  (cond ((member family +edge-families+) (worm.topology:family-size topology family))
        ((eq family :compartment) (1- (worm.topology:tube-rings topology)))
        ((eq family :ring) (worm.topology:tube-rings topology))
        ((eq family :node) (worm.topology:tube-node-count topology))
        ((eq family :muscle) (if binding (length (worm.anatomy:muscle-binding-muscles binding))
                                 (error 'model-error :message "семейство :muscle требует анатомии")))
        ((eq family :global) 1)
        (t (error 'model-error :message (format nil "семейство ~(~S~) интерпретатор пока не исполняет (этапы 10 и 12)" family)))))

(defun family-slot-map (family topology)
  "Для каждого элемента — вектор номеров узлов его слотов (в порядке FAMILY-SLOTS)."
  (let* ((kk (worm.topology:tube-ring-nodes topology))
         (count (and (or (member family +edge-families+) (member family '(:compartment :ring :node)))
                     (family-count family topology nil))))
    (cond ((member family +edge-families+)
           (multiple-value-bind (a b) (worm.topology:family-ends topology family)
             (let ((v (make-array count)))
               (dotimes (e count v) (setf (aref v e) (vector (aref a e) (aref b e)))))))
          ((eq family :compartment)
           (let ((v (make-array count)))
             (dotimes (c count v) (setf (aref v c) (coerce (loop for j below (* 2 kk) collect (+ (* c kk) j)) 'vector)))))
          ((eq family :ring)
           (let ((v (make-array count)))
             (dotimes (i count v) (setf (aref v i) (coerce (loop for j below kk collect (+ (* i kk) j)) 'vector)))))
          ((eq family :node)
           ;; сам узел, затем узлы предыдущего и следующего колец (у концов — своё)
           (let ((v (make-array count)) (rings (worm.topology:tube-rings topology)))
             (dotimes (n count v)
               (let* ((i (floor n kk)) (lo (max 0 (1- i))) (hi (min (1- rings) (1+ i))))
                 (setf (aref v n) (coerce (append (list n)
                                                  (loop for j below kk collect (+ (* lo kk) j))
                                                  (loop for j below kk collect (+ (* hi kk) j)))
                                          'vector))))))
          (t nil))))

(defun family-elements (family topology binding)
  "Величины элемента: ((имя . dvec) …)."
  (let ((kk (worm.topology:tube-ring-nodes topology))
        (frac (worm.topology:tube-ring-fractions topology)))
    (cond ((member family +edge-families+)
           (list (cons :rest-length (worm.topology:family-rest topology family))))
          ((eq family :compartment)
           (list (cons :rest-volume (worm.topology:tube-rest-volume topology))))
          ((eq family :ring)
           (list (cons :ds (worm.topology:tube-ring-ds topology))
                 (cons :rest-radius (worm.topology:tube-ring-radius topology))
                 (cons :position frac)))
          ((eq family :node)
           (let* ((n (worm.topology:tube-node-count topology))
                  (ds (make-dvec n)) (pos (make-dvec n)))
             (dotimes (i n)
               (setf (aref ds i) (/ (aref (worm.topology:tube-ring-ds topology) (floor i kk)) kk)
                     (aref pos i) (aref frac (floor i kk))))
             (list (cons :ds ds) (cons :position pos))))
          ((eq family :muscle)
           (let ((muscles (worm.anatomy:muscle-binding-muscles binding)))
             (list (cons :position (map 'dvec #'worm.anatomy:muscle-peak muscles))
                   (cons :side (map 'dvec (lambda (m) (if (member (worm.anatomy:muscle-quadrant m)
                                                                  '(:dorsal-left :dorsal-right))
                                                          1d0 -1d0))
                                    muscles)))))
          (t '()))))

(defun topology-value (form key params default)
  (let ((v (getf (rest form) key default)))
    (cond ((realp v) v)
          ((and (symbolp v) v) (worm.params:param-value params v))
          (t default))))

(defun relation-csr (wire src dst binding)
  "Для каждого элемента-получателя — список (источник . вес)."
  (declare (ignore src))
  (let ((n-dst (running-count dst)) (csr nil))
    (setf csr (make-array n-dst :initial-element '()))
    (ecase (wire-via wire)
      (:identity (dotimes (e n-dst) (setf (aref csr e) (list (cons e 1d0)))))
      (:broadcast (dotimes (e n-dst) (setf (aref csr e) (list (cons 0 1d0)))))
      (:attachment
       (let ((offsets (worm.anatomy:muscle-binding-offsets binding))
             (edges (worm.anatomy:muscle-binding-edges binding))
             (weights (worm.anatomy:muscle-binding-weights binding))
             (to-edges (eq (instance-family (running-instance dst)) :long-edge)))
         (dotimes (m (1- (length offsets)))
           (loop for q from (aref offsets m) below (aref offsets (1+ m))
                 do (if to-edges
                        (push (cons m (aref weights q)) (aref csr (aref edges q)))
                        (push (cons (aref edges q) (aref weights q)) (aref csr m)))))
         (dotimes (e n-dst) (setf (aref csr e) (nreverse (aref csr e)))))))
    csr))

(defun instance-order (instances wiring)
  "Экземпляры так, чтобы источник связи шёл раньше получателя. Учитываются
только связи внутри переданных экземпляров: петли между подсистемами
разрывает расщепление шага."
  (let ((done '()) (active '()))
    (labels ((visit (inst)
               (cond ((member (instance-name inst) done) nil)
                     ((member (instance-name inst) active)
                      (error 'model-error :message (format nil "связи экземпляров образуют петлю через ~(~A~)"
                                                           (instance-name inst))))
                     (t (push (instance-name inst) active)
                        (dolist (w wiring)
                          (when (eq (wire-dst-instance w) (instance-name inst))
                            (let ((src (find (wire-src-instance w) instances :key #'instance-name)))
                              (when src (visit src)))))
                        (pop active)
                        (setf done (append done (list (instance-name inst))))))))
      (mapc #'visit instances))
    (mapcar (lambda (n) (find n instances :key #'instance-name)) done)))

(defun subsystem-order (spec active wiring)
  "Экземпляры механики и нервно-мышечной части, каждые в порядке связей."
  (flet ((members (name)
           (let ((names (rest (assoc name (model-spec-subsystems spec)))))
             (remove-if-not (lambda (inst) (member (instance-name inst) names)) active))))
    (let ((mech (members :mechanical))
          (others (remove-if (lambda (inst) (member (instance-name inst)
                                                    (rest (assoc :mechanical (model-spec-subsystems spec)))))
                             active)))
      (values (instance-order mech wiring) (instance-order others wiring)))))

(defun instantiate (spec &key (params "water-v1") medium rings ring-nodes element-arrays)
  "Собрать модель по SPEC для интерпретации. PARAMS — набор параметров;
RINGS и RING-NODES заменяют значения из :topology; ELEMENT-ARRAYS —
((имя . dvec) …) вместо вычисляемых поэлементных массивов."
  (let* ((set (if (worm.params:param-set-p params) params (worm.params:load-param-set params)))
         (topo-form (model-spec-topology spec))
         (topology (worm.topology:make-tube
                    :rings (or rings (topology-value topo-form :rings set 50))
                    :ring-nodes (or ring-nodes (topology-value topo-form :ring-nodes set 8))
                    :length (float (topology-value topo-form :length set 1d0) 0d0)
                    :radius-max (float (topology-value topo-form :radius-max set 0.04d0) 0d0)
                    :radius-end-min (float (topology-value topo-form :radius-end-min set 0.005d0) 0d0)))
         (kk (worm.topology:tube-ring-nodes topology))
         (anatomy (and (model-spec-anatomy spec)
                       (worm.anatomy:load-anatomy :muscles-path (merge-pathnames (model-spec-anatomy spec) (project-root)))))
         (binding (and anatomy
                       (worm.anatomy:bind-muscles (worm.anatomy:anatomy-muscles anatomy)
                                                  (worm.topology:tube-ring-fractions topology)
                                                  (lambda (q i) (worm.topology:quadrant-long-edges topology q i)))))
         (medium (or medium (if (worm.params:param-value set :contact) :agar :water)))
         (active (remove-if (lambda (inst) (let ((w (instance-when inst))) (and w (not (eq (second w) medium)))))
                            (model-spec-instances spec)))
         (wiring (remove-if-not (lambda (w) (and (find (wire-dst-instance w) active :key #'instance-name)
                                                 (or (eq (wire-src-instance w) :geometry)
                                                     (find (wire-src-instance w) active :key #'instance-name))))
                                (model-spec-wiring spec)))
         (running
           (loop for inst in (multiple-value-bind (mech neural) (subsystem-order spec active wiring)
                               (append mech neural))
                 collect
                 (let* ((family (instance-family inst))
                        (component (instance-component inst))
                        (kernel (instance-kernel inst kk))
                        (count (family-count family topology binding))
                        (r (make-running
                            :instance inst :kernel kernel :count count
                            :slots (family-slot-map family topology)
                            :elements (family-elements family topology binding)
                            :params (loop for (pname kind) in (component-params component)
                                          for global = (or (cdr (assoc pname (instance-renames inst))) pname)
                                          when (eq kind :scalar)
                                            collect (cons global (float (worm.params:param-value set global) 0d0)))
                            :arrays (loop for (pname kind) in (component-params component)
                                          for global = (or (cdr (assoc pname (instance-renames inst))) pname)
                                          when (eq kind :per-element)
                                            collect (cons global
                                                          (or (cdr (assoc global element-arrays))
                                                              (funcall (element-array-compute (find-element-array global))
                                                                       (list :topology topology :params set :anatomy anatomy
                                                                             :binding binding :family family :count count)))))
                            :discrete (loop for (name init) in (component-discrete component)
                                            collect (cons name (make-array count :element-type 'double-float
                                                                                 :initial-element (float init 0d0))))
                            :inputs (loop for (name nil default) in (component-inputs component)
                                          collect (cons name (make-array count :element-type 'double-float
                                                                               :initial-element (or default 0d0))))
                            :outputs (loop for (name) in (component-outputs component) collect (cons name (make-dvec count)))
                            :derivs (loop for (name) in (component-state component) collect (cons name (make-dvec count))))))
                   (worm.ir:with-graph ((kernel-graph kernel))
                     (setf (running-evaluator r) (worm.ir:compile-evaluator (kernel-roots kernel)))
                     (let ((roots (loop for (coef . rate) in (kernel-damping kernel)
                                        collect (list coef
                                                      (loop for v in (velocity-leaves rate)
                                                            collect (cons (rest (worm.ir:var-key v))
                                                                          (worm.ir:derivative rate v)))))))
                       (setf (running-damping-roots r) roots
                             (running-damping-evaluator r)
                             (worm.ir:compile-evaluator
                              (loop for (coef grads) in roots append (cons coef (mapcar #'cdr grads))))))
                     ;; начальное состояние: значения выражений :state по элементам
                     (setf (running-states r)
                           (loop for (name . node) in (kernel-state-inits kernel)
                                 collect (cons name
                                               (let ((ev (worm.ir:compile-evaluator (list node)))
                                                     (v (make-dvec count)))
                                                 (dotimes (e count v)
                                                   (setf (aref v e) (first (worm.ir:run-evaluator ev (element-env r e nil nil 0d0))))))))))
                   r))))
    (make-model-instance
     :spec spec :topology topology :anatomy anatomy :binding binding :medium medium
     :mechanical (rest (assoc :mechanical (model-spec-subsystems spec)))
     :n-mech (* 3 (worm.topology:tube-node-count topology))
     :running running
     :relations (loop for w in wiring
                      unless (eq (wire-src-instance w) :geometry)
                        collect (cons w (relation-csr w
                                                      (find (wire-src-instance w) running :key (lambda (r) (instance-name (running-instance r))))
                                                      (find (wire-dst-instance w) running :key (lambda (r) (instance-name (running-instance r))))
                                                      binding))))))

(defun element-env (r e y aux time)
  "Функция ключ → число для элемента E экземпляра R."
  (let ((slots (and (running-slots r) (aref (running-slots r) e))))
    (lambda (key)
      (flet ((slot-node (slot)
               (let ((family (instance-family (running-instance r))))
                 (aref slots (if (integerp slot) slot (position slot (family-slots family 0)))))))
        (ecase (first key)
          (:pos (aref y (+ (* 3 (slot-node (second key))) (third key))))
          (:aux (if aux (aref aux (+ (* 3 (slot-node (second key))) (third key))) 0d0))
          (:vel (error 'model-error :message "скорость узлов вне :damping"))
          (:param (cdr (assoc (second key) (running-params r))))
          (:array (aref (cdr (assoc (second key) (running-arrays r))) e))
          (:state (aref (cdr (assoc (second key) (running-states r))) e))
          (:discrete (aref (cdr (assoc (second key) (running-discrete r))) e))
          (:input (aref (cdr (assoc (second key) (running-inputs r))) e))
          (:element (aref (cdr (assoc (second key) (running-elements r))) e))
          (:time time))))))

(defstruct (interpretation (:copier nil))
  forces outputs derivs)

(defun running-named (model name)
  (find name (model-instance-running model) :key (lambda (r) (instance-name (running-instance r)))))

(defun evaluate-running (model r y aux time forces)
  "Обновить входы R по связям и вычислить его ядро на всех элементах; силы —
в FORCES, если он задан."
  (let* ((inst (running-instance r))
         (kernel (running-kernel r))
         (n-out (length (kernel-outputs kernel)))
         (n-force (length (kernel-forces kernel)))
         (out (make-dvec (length (kernel-roots kernel)))))
    (loop for (name . v) in (running-inputs r)
          do (let ((default (third (assoc name (component-inputs (instance-component inst))))))
               (fill v (or default 0d0))))
    (loop for (w . csr) in (model-instance-relations model)
          when (eq (wire-dst-instance w) (instance-name inst))
            do (let ((src (cdr (assoc (wire-src-port w)
                                      (running-outputs (running-named model (wire-src-instance w))))))
                     (dst (cdr (assoc (wire-dst-port w) (running-inputs r)))))
                 (dotimes (e (length dst))
                   (setf (aref dst e) (loop for (s . wt) in (aref csr e) sum (* wt (aref src s)))))))
    (dotimes (e (running-count r))
      (worm.ir:run-evaluator (running-evaluator r) (element-env r e y aux time) out)
      (loop for (name) in (kernel-outputs kernel) for k from 0
            do (setf (aref (cdr (assoc name (running-outputs r))) e) (aref out k)))
      (when forces
        (loop for (slot c) in (kernel-forces kernel) for k from n-out
              do (let ((node (aref (aref (running-slots r) e)
                                   (if (integerp slot) slot (position slot (family-slots (instance-family inst) 0))))))
                   (incf (aref forces (+ (* 3 node) c)) (aref out k)))))
      (loop for (name) in (kernel-derivs kernel) for k from (+ n-out n-force)
            do (setf (aref (cdr (assoc name (running-derivs r))) e) (aref out k))))))

(defun interpret (model y &key aux (time 0d0) states)
  "Модель при координатах Y в порядке шага решателя (архитектура, «Порядок
одного шага»): выходы механики (наблюдаемые) по Y и AUX, затем
нервно-мышечная часть — её выходы и производные состояния в момент TIME, затем
силы механики с этими выходами. STATES — ((экземпляр (переменная . dvec) …) …)
вместо текущих значений состояния."
  (let ((forces (make-dvec (model-instance-n-mech model)))
        (mech (remove-if-not (lambda (r) (member (instance-name (running-instance r)) (model-instance-mechanical model)))
                             (model-instance-running model)))
        (others (remove-if (lambda (r) (member (instance-name (running-instance r)) (model-instance-mechanical model)))
                           (model-instance-running model))))
    (loop for (inst-name . vars) in states
          do (let ((r (running-named model inst-name)))
               (loop for (var . v) in vars
                     do (setf (cdr (assoc var (running-states r))) (dvec-from v)))))
    (dolist (r mech) (evaluate-running model r y aux time nil))
    (dolist (r others) (evaluate-running model r y aux time nil))
    (dolist (r mech) (evaluate-running model r y aux time forces))
    (make-interpretation
     :forces forces
     :outputs (loop for r in (model-instance-running model)
                    collect (cons (instance-name (running-instance r)) (running-outputs r)))
     :derivs (loop for r in (model-instance-running model)
                   collect (cons (instance-name (running-instance r)) (running-derivs r))))))

(defun interpret-damping (model y &key (time 0d0))
  "Матрица вязкости D (плотная, n × n) из членов :damping при координатах Y."
  (let* ((n (model-instance-n-mech model))
         (d (make-array (list n n) :element-type 'double-float :initial-element 0d0)))
    (dolist (r (model-instance-running model) d)
      (let* ((roots (running-damping-roots r))
             (family (instance-family (running-instance r)))
             (vals (make-dvec (loop for (nil grads) in roots sum (1+ (length grads))))))
        (when roots
          (dotimes (e (running-count r))
            (worm.ir:run-evaluator (running-damping-evaluator r) (element-env r e y nil time) vals)
            (let ((k 0))
              (dolist (term roots)
                (let* ((coef (aref vals k))
                       (g (loop for entry in (second term) for j from (1+ k)
                                for (slot c) = (car entry)
                                collect (cons (+ (* 3 (aref (aref (running-slots r) e)
                                                            (if (integerp slot) slot (position slot (family-slots family 0)))))
                                                 c)
                                              (aref vals j)))))
                  (loop for (i . gi) in g
                        do (loop for (j . gj) in g do (incf (aref d i j) (* coef gi gj))))
                  (incf k (1+ (length (second term)))))))))))))

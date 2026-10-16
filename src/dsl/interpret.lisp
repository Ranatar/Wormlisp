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

(defun anatomy-connectome-or-error (anatomy)
  (or (and anatomy (worm.anatomy:anatomy-connectome anatomy))
      (error 'model-error :message "семейство :neuron требует коннектома: anatomy/neurons.sexp и synapses.sexp рядом с файлом мышц")))

(defun family-count (family topology binding &optional params anatomy)
  (cond ((member family +edge-families+) (worm.topology:family-size topology family))
        ((eq family :compartment) (1- (worm.topology:tube-rings topology)))
        ((eq family :ring) (worm.topology:tube-rings topology))
        ((eq family :node) (worm.topology:tube-node-count topology))
        ((eq family :muscle) (if binding (length (worm.anatomy:muscle-binding-muscles binding))
                                 (error 'model-error :message "семейство :muscle требует анатомии")))
        ((eq family :global) 1)
        ((eq family :prop-unit)
         (if params (round (worm.params:param-value params :units))
             (error 'model-error :message "семейство :prop-unit требует параметра units")))
        ((eq family :neuron) (length (worm.anatomy:connectome-neurons (anatomy-connectome-or-error anatomy))))
        (t (error 'model-error :message (format nil "семейство ~(~S~) интерпретатор не исполняет" family)))))

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

(defun dorsal-node-p (k ring-nodes)
  (member (worm.topology:node-quadrant k ring-nodes) '(:dorsal-left :dorsal-right)))

(defparameter +dorsal-neuron-classes+ '(:da :db :dd :as))
(defparameter +ventral-neuron-classes+ '(:va :vb :vd :vc))

(defparameter +command-classes+ '(:ava :avb :avd :ave :pvc))
(defparameter +cord-classes+ '(:as :da :db :dd :va :vb :vc :vd :pda :pdb))

(defun command-neuron-p (neuron) (member (worm.anatomy:neuron-class neuron) +command-classes+))

(defun scaled-gaps (connectome params)
  "Щелевые контакты ((a b вес) …) с масштабом класса связи: gap-command-scale
у связей командного интернейрона (AVA, AVB, AVD, AVE, PVC) с остальными,
gap-motor-scale — у прочих (параметры, «Уровень 3»)."
  (let ((neurons (worm.anatomy:connectome-neurons connectome))
        (command (float (worm.params:param-value params :gap-command-scale) 0d0))
        (motor (float (worm.params:param-value params :gap-motor-scale) 0d0)))
    (loop for (a b c) across (worm.anatomy:connectome-gap connectome)
          for ca = (command-neuron-p (aref neurons a)) for cb = (command-neuron-p (aref neurons b))
          collect (list a b (* c (if (and (or ca cb) (not (and ca cb))) command motor))))))

(defun scaled-chemical (connectome params)
  "Химические синапсы ((пре пост вес) …), вес — контакты × chemical-scale."
  (let ((k (float (worm.params:param-value params :chemical-scale) 0d0)))
    (loop for (a b c) across (worm.anatomy:connectome-chemical connectome) collect (list a b (* k c)))))

(defun neuron-elements (connectome params)
  (let* ((neurons (worm.anatomy:connectome-neurons connectome))
         (n (length neurons))
         (gap-total (make-dvec n)))
    (loop for (a b c) in (scaled-gaps connectome params)
          do (incf (aref gap-total a) c) (incf (aref gap-total b) c))
    (flet ((column (f) (map 'dvec (lambda (x) (float (funcall f x) 0d0)) neurons)))
      (list (cons :position (column (lambda (x) (or (worm.anatomy:neuron-position x) 0.5d0))))
            (cons :side (column (lambda (x) (let ((c (worm.anatomy:neuron-class x)))
                                              (cond ((member c +dorsal-neuron-classes+) 1)
                                                    ((member c +ventral-neuron-classes+) -1)
                                                    (t 0))))))
            (cons :gaba (column (lambda (x) (if (eq (worm.anatomy:neuron-transmitter x) :gaba) 1 0))))
            (cons :proprioceptive (column (lambda (x) (if (member (worm.anatomy:neuron-class x) '(:db :vb)) 1 0))))
            (cons :command (column (lambda (x) (case (worm.anatomy:neuron-class x) (:avb 1) (:ava -1) (t 0)))))
            (cons :cord (column (lambda (x) (if (member (worm.anatomy:neuron-class x) +cord-classes+) 1 0))))
            (cons :gap-total gap-total)))))

(defun family-elements (family topology binding &optional params anatomy)
  "Величины элемента: ((имя . dvec) …)."
  (let ((kk (worm.topology:tube-ring-nodes topology))
        (frac (worm.topology:tube-ring-fractions topology)))
    (cond ((eq family :long-edge)
           ;; сторона (+1 дорсальная, −1 вентральная) и R_max / R отрезка:
           ;; относительное удлинение стороны, приведённое к толщине тела
           (let* ((n (worm.topology:family-size topology family))
                  (radius (worm.topology:tube-ring-radius topology))
                  (rmax (worm.topology:tube-radius-max topology))
                  (side (make-dvec n)) (factor (make-dvec n)) (pos (make-dvec n)))
             (dotimes (e n)
               (multiple-value-bind (i k) (floor e kk)
                 (setf (aref side e) (if (dorsal-node-p k kk) 1d0 -1d0)
                       (aref factor e) (/ rmax (* 0.5d0 (+ (aref radius i) (aref radius (1+ i)))))
                       (aref pos e) (* 0.5d0 (+ (aref frac i) (aref frac (1+ i)))))))
             (list (cons :rest-length (worm.topology:family-rest topology family))
                   (cons :side side) (cons :radius-factor factor) (cons :position pos))))
          ((member family +edge-families+)
           (list (cons :rest-length (worm.topology:family-rest topology family))))
          ((eq family :prop-unit)
           (let* ((m (family-count family topology binding params))
                  (index (make-dvec m)) (pos (make-dvec m)))
             (dotimes (u m)
               (setf (aref index u) (float (1+ u) 0d0)
                     (aref pos u) (/ (+ u 0.5d0) m)))
             (list (cons :index index) (cons :position pos))))
          ((eq family :neuron) (neuron-elements (anatomy-connectome-or-error anatomy) params))
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

(defun unit-of-position (s units)
  "Единица (0…M−1), участку которой принадлежит положение S (доля L)."
  (max 0 (min (1- units) (floor (* s units)))))

(defun receptive-field (unit units params)
  "Поле единицы UNIT: (начало конец направление) в долях L. Назад — от
начала участка единицы к хвосту (участок самой единицы входит в поле),
вперёд — от начала участка к голове (участок впереди, Wen 2012); длина
rf-length. При поле вперёд у первой
единицы впереди ничего нет: она — головной осциллятор с полем назад, как
у Бойла."
  (let ((len (float (worm.params:param-value params :rf-length) 0d0))
        (start (/ unit units))
        (direction (worm.params:param-value params :rf-direction)))
    (cond ((eq direction :posterior) (list start (min 1d0 (+ start len)) :posterior))
          ;; головной генератор при поле вперёд: своё поле назад длиной head-rf-length
          ((zerop unit)
           (list start (min 1d0 (+ start (float (worm.params:param-value params :head-rf-length) 0d0))) :posterior))
          (t (list (max 0d0 (- start len)) start :anterior)))))

(defun receptive-field-csr (src csr n-dst params)
  "Поле проприоцептивной единицы: сумма по полю, нормированная на число
рёбер одной стороны в нём: вес N_SR / n, N_SR = 48·rf-length — число
сегментов модели Бойла."
  (let* ((pos (cdr (assoc :position (running-elements src))))
              (n-sr (* 48 (float (worm.params:param-value params :rf-length) 0d0))))
         (dotimes (u n-dst)
           (destructuring-bind (a b direction) (receptive-field u n-dst params)
             (let ((edges (loop for e below (length pos)
                                when (<= a (aref pos e) b) collect e)))
               (when edges
                 ;; дорсальных и вентральных рёбер в поле поровну. Поле вперёд
                 ;; (Wen 2012) повторяет изгиб участка впереди: B-нейрон
                 ;; стороны возбуждается сжатием этой стороны, поэтому знак
                 ;; сигнала обратный
                 (let ((w (* (if (eq direction :anterior) -1 1)
                             (/ n-sr (/ (length edges) 2)))))
                   (setf (aref csr u) (mapcar (lambda (e) (cons e w)) edges))))))))
  csr)

(defun innervation-units-csr (src dst csr n-dst params)
  "Мышца — от проприоцептивной единицы своего участка, вес — сторона мышцы.
При поле вперёд мышцы головы до head-muscles-end (доля L) — от головной
единицы (головной генератор)."
  (let ((units (running-count src))
        (side (cdr (assoc :side (running-elements dst))))
        (pos (cdr (assoc :position (running-elements dst))))
        (head-end (if (eq (worm.params:param-value params :rf-direction) :anterior)
                      (float (worm.params:param-value params :head-muscles-end) 0d0)
                      0d0)))
    (dotimes (m n-dst)
      (setf (aref csr m) (list (cons (if (< (aref pos m) head-end) 0 (unit-of-position (aref pos m) units))
                                     (aref side m))))))
  csr)

(defun relation-csr (wire src dst binding &optional topology params anatomy)
  "Для каждого элемента-получателя — список (источник . вес)."
  (declare (ignore topology))
  (let ((n-dst (running-count dst)) (csr nil))
    (setf csr (make-array n-dst :initial-element '()))
    (ecase (wire-via wire)
      ((:receptive-field)
       (if (eq (instance-family (running-instance dst)) :neuron)
           ;; поле B-нейрона длиной rf-length назад или вперёд от его положения,
           ;; тот же вес; поле вперёд (Wen 2012) — с обратным знаком, как у единиц
           (let* ((pos (cdr (assoc :position (running-elements src))))
                  (len (float (worm.params:param-value params :rf-length) 0d0))
                  (anterior (eq (worm.params:param-value params :rf-direction) :anterior))
                  (n-sr (* 48 len))
                  (npos (cdr (assoc :position (running-elements dst))))
                  (prop (cdr (assoc :proprioceptive (running-elements dst)))))
             (dotimes (u n-dst)
               (when (plusp (aref prop u))
                 (let* ((a (if anterior (max 0d0 (- (aref npos u) len)) (aref npos u)))
                        (b (if anterior (aref npos u) (min 1d0 (+ (aref npos u) len))))
                        (edges (loop for e below (length pos) when (<= a (aref pos e) b) collect e)))
                   (when edges
                     (let ((w (* (if anterior -1 1) (/ n-sr (/ (length edges) 2)))))
                       (setf (aref csr u) (mapcar (lambda (e) (cons e w)) edges))))))))
           (receptive-field-csr src csr n-dst params)))
      (:chemical
       (loop for (pre post c) in (scaled-chemical (anatomy-connectome-or-error anatomy) params)
             do (push (cons pre c) (aref csr post)))
       (dotimes (e n-dst) (setf (aref csr e) (nreverse (aref csr e)))))
      (:gap
       (loop for (a b c) in (scaled-gaps (anatomy-connectome-or-error anatomy) params)
             do (push (cons b c) (aref csr a)) (push (cons a c) (aref csr b)))
       (dotimes (e n-dst) (setf (aref csr e) (nreverse (aref csr e)))))
      (:innervation
       (if (eq (instance-family (running-instance src)) :neuron)
           ;; нервно-мышечные синапсы коннектома, вес — доля контактов: число
           ;; контактов, делённое на сумму возбуждающих контактов мотонейронов
           ;; тяжа на эту мышцу (у мышц без них — всех возбуждающих)
           (let* ((connectome (anatomy-connectome-or-error anatomy))
                  (neurons (worm.anatomy:connectome-neurons connectome))
                  (nmj (worm.anatomy:connectome-nmj connectome))
                  (cord (make-dvec n-dst)) (all (make-dvec n-dst)))
             (loop for (j m c) across nmj
                   for x = (aref neurons j)
                   unless (eq (worm.anatomy:neuron-transmitter x) :gaba)
                     do (incf (aref all m) c)
                        (when (member (worm.anatomy:neuron-class x) +cord-classes+) (incf (aref cord m) c)))
             (loop for (j m c) across nmj
                   do (push (cons j (/ c (if (plusp (aref cord m)) (aref cord m) (max 1d0 (aref all m)))))
                            (aref csr m)))
             (dotimes (e n-dst) (setf (aref csr e) (nreverse (aref csr e)))))
           (innervation-units-csr src dst csr n-dst params)))
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
                          ;; связь экземпляра с самим собой (нейроны коннектома)
                          ;; порядка не задаёт: её выходы зависят только от состояния
                          (when (and (eq (wire-dst-instance w) (instance-name inst))
                                     (not (eq (wire-src-instance w) (instance-name inst))))
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
                        (count (family-count family topology binding set anatomy))
                        (r (make-running
                            :instance inst :kernel kernel :count count
                            :slots (family-slot-map family topology)
                            :elements (family-elements family topology binding set anatomy)
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
    ;; выход, который экземпляр передаёт сам себе, не должен зависеть от входов
    (dolist (w wiring)
      (when (eq (wire-src-instance w) (wire-dst-instance w))
        (let ((r (find (wire-src-instance w) running :key (lambda (r) (instance-name (running-instance r))))))
          (when (cdr (assoc (wire-src-port w) (kernel-output-deps (running-kernel r))))
            (error 'model-error :message (format nil "выход ~(~A.~A~) идёт в тот же экземпляр и зависит от его входов"
                                                 (wire-src-instance w) (wire-src-port w)))))))
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
                                                      binding topology set anatomy))))))

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
  forces outputs derivs discrete)

(defun running-named (model name)
  (find name (model-instance-running model) :key (lambda (r) (instance-name (running-instance r)))))

(defun evaluate-running (model r y aux time forces &optional events)
  "Обновить входы R по связям и вычислить его ядро на всех элементах; силы —
в FORCES, если он задан; при EVENTS — применить события к дискретному
состоянию (все условия элемента — по состоянию до применения)."
  (let* ((inst (running-instance r))
         (kernel (running-kernel r))
         (n-out (length (kernel-outputs kernel)))
         (n-force (length (kernel-forces kernel)))
         (out (make-dvec (length (kernel-roots kernel)))))
    ;; связь экземпляра с самим собой: выходы по состоянию — раньше входов
    (when (some (lambda (rel) (and (eq (wire-src-instance (car rel)) (instance-name inst))
                                   (eq (wire-dst-instance (car rel)) (instance-name inst))))
                (model-instance-relations model))
      (dotimes (e (running-count r))
        (worm.ir:run-evaluator (running-evaluator r) (element-env r e y aux time) out)
        (loop for (name) in (kernel-outputs kernel) for k from 0
              do (setf (aref (cdr (assoc name (running-outputs r))) e) (aref out k)))))
    (loop for (name . v) in (running-inputs r)
          do (let ((default (third (assoc name (component-inputs (instance-component inst))))))
               (fill v (or default 0d0))))
    (loop for (w . csr) in (model-instance-relations model)
          when (eq (wire-dst-instance w) (instance-name inst))
            do (let ((src (cdr (assoc (wire-src-port w)
                                      (running-outputs (running-named model (wire-src-instance w))))))
                     (dst (cdr (assoc (wire-dst-port w) (running-inputs r)))))
                 (dotimes (e (length dst))
                   (setf (aref dst e) (float (loop for (s . wt) in (aref csr e) sum (* wt (aref src s))) 0d0)))))
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
            do (setf (aref (cdr (assoc name (running-derivs r))) e) (aref out k)))
      (when events
        (let ((base (+ n-out n-force (length (kernel-derivs kernel)))))
          (loop for (direction var) in (kernel-events kernel) for k from base by 2
                for c = (aref out k)
                when (if (eq direction :rising) (> c 0) (< c 0))
                  collect (cons var (aref out (1+ k))) into sets
                finally (loop for (var . v) in sets
                              do (setf (aref (cdr (assoc var (running-discrete r))) e) v))))))))

(defun interpret (model y &key aux (time 0d0) states events)
  "Модель при координатах Y в порядке шага решателя (архитектура, «Порядок
одного шага»): выходы механики (наблюдаемые) по Y и AUX, затем
нервно-мышечная часть — её выходы и производные состояния в момент TIME, затем
силы механики с этими выходами. STATES — ((экземпляр (переменная . dvec) …) …)
вместо текущих значений состояния. EVENTS — перед нервно-мышечной частью
применить события к дискретному состоянию, как neural-discrete решателя."
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
    (when events
      (dolist (r others) (evaluate-running model r y aux time nil (kernel-events (running-kernel r)))))
    (dolist (r others) (evaluate-running model r y aux time nil))
    (dolist (r mech) (evaluate-running model r y aux time forces))
    (make-interpretation
     :forces forces
     :outputs (loop for r in (model-instance-running model)
                    collect (cons (instance-name (running-instance r)) (running-outputs r)))
     :derivs (loop for r in (model-instance-running model)
                   collect (cons (instance-name (running-instance r)) (running-derivs r)))
     :discrete (loop for r in (model-instance-running model)
                     when (running-discrete r)
                       collect (cons (instance-name (running-instance r)) (running-discrete r))))))

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

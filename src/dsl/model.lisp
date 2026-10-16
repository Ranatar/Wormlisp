;;;; src/dsl/model.lisp — defmodel: раскрытие, проверки, model-spec.
;;;;
;;;;   (defmodel <имя>
;;;;     :topology   (tube :rings 50 :ring-nodes 8 :length body-length
;;;;                       :radius-max radius-max :radius-end-min radius-end-min)
;;;;     :anatomy    "anatomy/muscles.sexp"          ; от корня проекта; NIL — без анатомии
;;;;     :components ((<экземпляр> <компонент> :on <семейство>
;;;;                    [:rename (<параметр> <имя-в-модели> …)] [:when (medium :agar)]) …)
;;;;     :wiring     (((<экземпляр> <выход>) (<экземпляр> <вход>) :via <отношение>) …)
;;;;     :subsystems ((:mechanical <экземпляр> …) (:neural <экземпляр> …)))
;;;;
;;;; Проверки при раскрытии (архитектура, «Проверки при раскрытии
;;;; defmodel»): компоненты определены, семейства совпадают, параметры есть
;;;; в реестре, анатомия загружается, каждый обязательный вход связан ровно
;;;; один раз, отношение :via подходит семействам обоих концов, связи внутри
;;;; подсистемы не образуют алгебраических петель. Ошибка — условие с
;;;; указанием экземпляра и формы.
;;;;
;;;; Результат — model-spec с каноническим текстом: модель и определения
;;;; всех её компонентов, напечатанные без пакетов и без зависимости от
;;;; порядка хэш-таблиц. Хэш модели — MD5 этого текста.

(in-package #:worm.dsl)

(defstruct (instance (:constructor %make-instance-spec) (:copier nil))
  name component family renames when source)

(defstruct (wire (:copier nil))
  src-instance src-port dst-instance dst-port via source)

(defstruct (model-spec (:copier nil))
  name topology anatomy instances wiring subsystems components
  canonical-text hash source)

(defvar *models* (make-hash-table :test 'eq))

(defun find-model (name &optional (errorp t))
  (or (gethash (kw name) *models*)
      (and errorp (error 'model-error :message (format nil "модель ~(~A~) не определена" name)))))

(defparameter +model-keys+ '(:topology :anatomy :components :wiring :subsystems :documentation))

;;; Экземпляры

(defun parse-instance (form names)
  (let ((*form* form)
        (*component* (and (consp form) (first form))))
    (unless (and (consp form) (symbolp (first form)) (first form) (symbolp (second form)) (second form))
      (dsl-error 'dsl-syntax-error ":components — (<экземпляр> <компонент> :on <семейство> …)"))
    (let ((name (kw (first form))))
      (when (member name names) (dsl-error 'dsl-syntax-error "экземпляр ~(~A~) объявлен дважды" name))
      (check-plist (cddr form) '(:on :rename :when) "экземпляр")
      (destructuring-bind (&key on rename when) (cddr form)
        (let ((component (find-component (second form))))
          (unless (member on +families+)
            (dsl-error 'family-mismatch "экземпляр ставится на неизвестное семейство ~S" on))
          (unless (family-accepts-p (component-family component) on)
            (dsl-error 'family-mismatch "компонент ~(~A~) семейства ~(~S~) нельзя поставить на ~(~S~)"
                       (component-name component) (component-family component) on))
          (unless (and (listp rename) (evenp (length rename)))
            (dsl-error 'dsl-syntax-error ":rename — список пар (<параметр> <имя-в-модели> …)"))
          (let ((renames (loop for (from to) on rename by #'cddr
                               collect (progn
                                         (unless (assoc (kw from) (component-params component))
                                           (let ((*form* rename))
                                             (dsl-error 'unknown-name "у компонента ~(~A~) нет параметра ~(~A~)"
                                                        (component-name component) from)))
                                         (cons (kw from) (kw to))))))
            (when (and when (not (and (consp when) (name-is (first when) "MEDIUM")
                                      (member (second when) '(:water :agar)) (null (cddr when)))))
              (let ((*form* when)) (dsl-error 'dsl-syntax-error ":when — (medium :water) или (medium :agar)")))
            ;; параметры экземпляра — в реестре
            (dolist (p (component-params component))
              (destructuring-bind (pname kind) p
                (let ((global (or (cdr (assoc pname renames)) pname)))
                  (if (eq kind :per-element)
                      (find-element-array global)
                      (handler-case (worm.params:find-parameter global)
                        (worm.params:unknown-parameter ()
                          (dsl-error 'model-unknown-parameter "параметра ~(~A~) нет в реестре~@[ (в компоненте — ~(~A~))~]"
                                     global (and (not (eq global pname)) pname))))))))
            (make-instance-struct name component on renames when form)))))))

(defun make-instance-struct (name component family renames when source)
  (%make-instance-spec :name name :component component :family family :renames renames :when when :source source))

;;; Связи

(defun parse-wire (form instances)
  (let ((*form* form) (*component* nil))
    (unless (and (consp form) (consp (first form)) (consp (second form)))
      (dsl-error 'dsl-syntax-error ":wiring — ((<экземпляр> <выход>) (<экземпляр> <вход>) :via <отношение>)"))
    (destructuring-bind (src dst &key via) form
      (unless (and (= (length src) 2) (= (length dst) 2))
        (dsl-error 'dsl-syntax-error ":wiring — ((<экземпляр> <выход>) (<экземпляр> <вход>) :via <отношение>)"))
      (let ((relation (or (find-relation via)
                          (dsl-error 'unknown-name "нет отношения ~S; известны ~{~S~^ ~}" via
                                     (mapcar #'relation-name +relations+))))
            (dst-inst (find (kw (first dst)) instances :key #'instance-name)))
        (unless dst-inst (dsl-error 'unknown-name "нет экземпляра ~(~A~)" (first dst)))
        (setf *component* (instance-name dst-inst))
        (unless (assoc (kw (second dst)) (component-inputs (instance-component dst-inst)))
          (dsl-error 'unknown-name "у экземпляра ~(~A~) нет входа ~(~A~)" (first dst) (second dst)))
        (multiple-value-bind (src-name src-family)
            (if (eq (first src) :geometry)
                (values :geometry :ring)
                (let ((inst (find (kw (first src)) instances :key #'instance-name)))
                  (unless inst (dsl-error 'unknown-name "нет экземпляра ~(~A~)" (first src)))
                  (unless (assoc (kw (second src)) (component-outputs (instance-component inst)))
                    (dsl-error 'unknown-name "у экземпляра ~(~A~) нет выхода ~(~A~)" (first src) (second src)))
                  (values (instance-name inst) (instance-family inst))))
          (unless (relation-allows-p relation src-family (instance-family dst-inst))
            (dsl-error 'family-mismatch "отношение ~S не связывает ~(~S~) с ~(~S~)"
                       via src-family (instance-family dst-inst)))
          (make-wire :src-instance src-name :src-port (kw (second src))
                     :dst-instance (instance-name dst-inst) :dst-port (kw (second dst))
                     :via via :source form))))))

(defun check-inputs (instances wiring)
  "Каждый обязательный вход связан ровно один раз, необязательный — не больше раза."
  (dolist (inst instances)
    (let ((*component* (instance-name inst)) (*form* (instance-source inst)))
      (dolist (in (component-inputs (instance-component inst)))
        (destructuring-bind (name tag default) in
          (declare (ignore tag))
          (let ((wires (remove-if-not (lambda (w) (and (eq (wire-dst-instance w) (instance-name inst))
                                                       (eq (wire-dst-port w) name)))
                                      wiring)))
            (cond ((and (null wires) (null default))
                   (error 'unbound-input :input name :component (instance-name inst) :form (instance-source inst)
                                         :message (format nil "вход ~(~A~) экземпляра ~(~A~) не связан"
                                                          name (instance-name inst))))
                  ((cdr wires)
                   (let ((*form* (wire-source (second wires))))
                     (dsl-error 'input-bound-twice "вход ~(~A~) экземпляра ~(~A~) связан ~D раза"
                                name (instance-name inst) (length wires)))))))))))

;;; Подсистемы и петли

(defun parse-subsystems (form instances)
  (let ((*form* form) (*component* nil) (seen '()))
    (unless (listp form) (dsl-error 'dsl-syntax-error ":subsystems — ((<подсистема> <экземпляр> …) …)"))
    (let ((out (mapcar (lambda (s)
                         (let ((*form* s))
                           (unless (and (consp s) (keywordp (first s)))
                             (dsl-error 'dsl-syntax-error "подсистема — (<ключевое слово> <экземпляр> …)"))
                           (cons (first s)
                                 (mapcar (lambda (i)
                                           (let ((name (kw i)))
                                             (unless (find name instances :key #'instance-name)
                                               (dsl-error 'unknown-name "в подсистеме ~(~S~) нет такого экземпляра ~(~A~)"
                                                          (first s) i))
                                             (when (member name seen)
                                               (dsl-error 'dsl-syntax-error "экземпляр ~(~A~) входит в две подсистемы" i))
                                             (push name seen)
                                             name))
                                         (rest s)))))
                       form)))
      (dolist (inst instances out)
        (unless (member (instance-name inst) seen)
          (let ((*component* (instance-name inst)))
            (dsl-error 'dsl-syntax-error "экземпляр ~(~A~) не входит ни в одну подсистему" (instance-name inst))))))))

(defun instance-kernel (inst ring-nodes)
  (lower-component (instance-component inst) :family (instance-family inst) :ring-nodes ring-nodes
                                             :renames (instance-renames inst) :instance (instance-name inst)))

(defun check-loops (instances wiring subsystems ring-nodes)
  "Связи внутри подсистемы без алгебраических петель: выход → вход по связи,
вход → выход того же экземпляра, если выход зависит от входа мгновенно."
  (let ((deps (loop for inst in instances
                    collect (cons (instance-name inst) (kernel-output-deps (instance-kernel inst ring-nodes))))))
    (dolist (sub subsystems)
      (let ((members (rest sub)) (graph (make-hash-table :test 'equal)))
        (flet ((edge (from to) (push to (gethash from graph))))
          (dolist (w wiring)
            (when (and (member (wire-src-instance w) members) (member (wire-dst-instance w) members))
              (edge (cons (wire-src-instance w) (wire-src-port w)) (cons (wire-dst-instance w) (wire-dst-port w)))))
          (dolist (m members)
            (loop for (out . ins) in (cdr (assoc m deps))
                  do (dolist (in ins) (edge (cons m in) (cons m out))))))
        ;; поиск цикла обходом в глубину в детерминированном порядке
        (let ((state (make-hash-table :test 'equal)) (path '()))
          (labels ((visit (v)
                     (case (gethash v state)
                       (:done nil)
                       (:active (progn
                                  (let* ((tail (member v (reverse path) :test #'equal))
                                         (names (mapcar (lambda (p) (format nil "~(~A.~A~)" (car p) (cdr p)))
                                                        (append tail (list v)))))
                                    (error 'algebraic-loop :cycle names :component (car v) :form (first sub)
                                                           :message (format nil "алгебраическая петля в подсистеме ~(~S~): ~{~A~^ → ~}"
                                                                            (first sub) names)))))
                       (t (setf (gethash v state) :active)
                          (push v path)
                          (dolist (w (sort (copy-list (gethash v graph)) #'string<
                                           :key (lambda (p) (format nil "~A.~A" (car p) (cdr p)))))
                            (visit w))
                          (pop path)
                          (setf (gethash v state) :done)))))
            (let ((starts '()))
              (maphash (lambda (k v) (declare (ignore v)) (push k starts)) graph)
              (dolist (s (sort starts #'string< :key (lambda (p) (format nil "~A.~A" (car p) (cdr p)))))
                (visit s)))))))))

;;; Канонический текст

(defun canonical-write (x stream)
  (typecase x
    (null (write-string "nil" stream))
    (keyword (write-char #\: stream) (write-string (string-downcase (symbol-name x)) stream))
    (symbol (write-string (string-downcase (symbol-name x)) stream))
    (string (prin1 x stream))
    (pathname (prin1 (namestring x) stream))
    (integer (format stream "~D" x))
    (ratio (format stream "~D/~D" (numerator x) (denominator x)))
    (float (let ((*read-default-float-format* 'double-float)) (prin1 (float x 0d0) stream)))
    (cons (write-char #\( stream)
          (loop for (a . rest) on x
                do (canonical-write a stream)
                   (cond ((consp rest) (write-char #\Space stream))
                         (rest (write-string " . " stream) (canonical-write rest stream))))
          (write-char #\) stream))
    (t (error 'dsl-syntax-error :message (format nil "в тексте модели недопустимое значение ~S" x)))))

(defun canonical-string (x)
  (with-output-to-string (s) (canonical-write x s)))

(defun canonical-model-text (spec)
  "Модель и определения её компонентов — без пакетов и в порядке, который не
зависит от хэш-таблиц: компоненты — по именам."
  (let* ((source (model-spec-source spec))
         (components (model-spec-components spec))
         (arrays (sort (remove-duplicates
                        (loop for inst in (model-spec-instances spec)
                              append (loop for (pname kind) in (component-params (instance-component inst))
                                           when (eq kind :per-element)
                                             collect (or (cdr (assoc pname (instance-renames inst))) pname))))
                       #'string< :key #'symbol-name)))
    (canonical-string
     (list :model (second source)
           :form (cddr source)
           :definitions (mapcar #'component-source components)
           :element-arrays arrays
           :language 1))))

;;; Раскрытие

(defun topology-ring-nodes (topology)
  (let ((k (and (consp topology) (getf (rest topology) :ring-nodes))))
    (if (integerp k) k 8)))

(defun parse-model (form)
  "model-spec из формы (defmodel имя …)."
  (let ((*component* nil) (*form* form))
    (unless (and (consp form) (symbolp (second form)) (second form))
      (dsl-error 'dsl-syntax-error "defmodel: ожидалось имя модели"))
    (check-plist (cddr form) +model-keys+ "defmodel")
    (destructuring-bind (&key topology anatomy components wiring subsystems documentation) (cddr form)
      (declare (ignore documentation))
      (unless (and (consp topology) (name-is (first topology) "TUBE"))
        (let ((*form* topology)) (dsl-error 'dsl-syntax-error ":topology — (tube :rings … :ring-nodes … …)")))
      (let ((instances '()))
        (dolist (c components)
          (setf instances (append instances (list (parse-instance c (mapcar #'instance-name instances))))))
        (when anatomy
          (let ((*form* anatomy) (*component* nil))
            (handler-case (worm.anatomy:load-anatomy :muscles-path (merge-pathnames anatomy (project-root)))
              (worm.anatomy:anatomy-invalid (e)
                (dsl-error 'model-anatomy-invalid "анатомия модели: ~A" (worm-error-message e)))
              (file-error ()
                (dsl-error 'model-anatomy-invalid "файла анатомии ~A нет" anatomy)))))
        (let* ((wires (mapcar (lambda (w) (parse-wire w instances)) wiring))
               (subs (parse-subsystems subsystems instances))
               (k (topology-ring-nodes topology)))
          (check-inputs instances wires)
          (check-loops instances wires subs k)
          (let ((spec (make-model-spec
                       :name (second form) :topology topology :anatomy anatomy
                       :instances instances :wiring wires :subsystems subs
                       :components (sort (remove-duplicates (mapcar #'instance-component instances))
                                         #'string< :key (lambda (c) (symbol-name (component-name c))))
                       :source form)))
            (setf (model-spec-canonical-text spec) (canonical-model-text spec)
                  (model-spec-hash spec) (worm.platform:md5-hex (model-spec-canonical-text spec)))
            spec))))))

(defun register-model (spec)
  (setf (gethash (kw (model-spec-name spec)) *models*) spec))

(defmacro defmodel (name &body plist)
  "Определить модель; возвращает model-spec. Ошибки — условия model-error с
указанием экземпляра и формы."
  `(register-model (parse-model '(defmodel ,name ,@plist))))

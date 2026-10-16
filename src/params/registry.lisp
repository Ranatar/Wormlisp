;;;; src/params/registry.lisp — реестр параметров.
;;;;
;;;; Каждый параметр модели зарегистрирован с именем, видом значения,
;;;; единицей (а через неё — размерностью), жёсткими границами во
;;;; внутренних единицах и признаком живой правки. Наборы параметров
;;;; проверяются по реестру; имя вне реестра — ошибка unknown-parameter.
;;;;
;;;; Виды значений:
;;;;   :real     число с единицей
;;;;   :integer  целое число без единицы (число единиц, колец)
;;;;   :vector   список чисел с общей единицей
;;;;   :boolean  T или NIL, без единицы
;;;;   :choice   ключевое слово из CHOICES, без единицы
;;;;
;;;; Живая правка разрешена параметрам, которые не входят в величины покоя
;;;; топологии (архитектура, «Параметры»): длина тела и профиль радиуса
;;;; требуют пересборки.

(in-package #:worm.params)

(defstruct (parameter-spec (:copier nil))
  (name nil :type keyword :read-only t)
  (kind :real :type (member :real :integer :vector :boolean :choice) :read-only t)
  (unit nil :read-only t)  ; Единица, задающая размерность; NIL — безразмерная.
  (dimension '(0 0 0 0 0) :read-only t)
  (bounds nil :read-only t)  ; (min max) во внутренних единицах или NIL.
  (live-p t :read-only t)
  (choices nil :read-only t)
  (doc "" :read-only t))

(defvar *registry* (make-hash-table :test 'eq))
(defvar *registry-lock* (worm.platform:make-lock "worm params registry"))

(defun normalize-name (name)
  "Имя параметра как ключевое слово: 'TAU-CA, :tau-ca и \"tau-ca\" — одно имя."
  (etypecase name
    (keyword name)
    (symbol (intern (symbol-name name) :keyword))
    (string (intern (string-upcase name) :keyword))))

(defun register-parameter (name &key (kind :real) unit bounds (live t) choices (doc ""))
  (let* ((key (normalize-name name))
         (spec (make-parameter-spec
                :name key :kind kind :unit unit
                :dimension (unit-dimension unit)
                :bounds (and bounds
                             (list (to-internal (first bounds) unit)
                                   (to-internal (second bounds) unit)))
                :live-p live :choices choices :doc doc)))
    (when (and (member kind '(:integer :boolean :choice)) unit)
      (error "параметр ~(~A~) вида ~(~A~) не может иметь единицу" key kind))
    (worm.platform:with-lock (*registry-lock*)
      (setf (gethash key *registry*) spec))
    key))

(defmacro define-parameter (name &rest options
                            &key kind unit bounds live choices doc)
  "Зарегистрировать параметр NAME. BOUNDS задаются в единицах UNIT."
  (declare (ignore kind unit bounds live choices doc))
  `(register-parameter ',name ,@(loop for (k v) on options by #'cddr
                                      append (list k (if (member k '(:unit :choices :bounds))
                                                         `',v v)))))

(defun find-parameter (name &optional (errorp t))
  (or (gethash (normalize-name name) *registry*)
      (and errorp
           (error 'unknown-parameter
                  :message (format nil "параметра ~(~A~) нет в реестре" (normalize-name name))))))

(defun list-parameters ()
  "Все зарегистрированные параметры, по алфавиту."
  (sort (loop for spec being the hash-values of *registry* collect spec)
        #'string< :key (lambda (s) (symbol-name (parameter-spec-name s)))))

(defun clear-registry ()
  (worm.platform:with-lock (*registry-lock*)
    (clrhash *registry*)))

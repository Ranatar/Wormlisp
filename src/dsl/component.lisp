;;;; src/dsl/component.lisp — define-component: разбор, проверка, реестр.
;;;;
;;;;   (define-component <имя>
;;;;     :family   <семейство>
;;;;     :params   (<скаляр> … (<массив> :per-element) …)
;;;;     :state    ((<переменная> <начальное выражение>) …)
;;;;     :discrete ((<переменная> <начальное число>) …)
;;;;     :inputs   ((<имя> (link <метка>)) (<имя> (link <метка>) :default <число>) …)
;;;;     :let      ((<имя> <выражение>) …)
;;;;     :derivs   ((<переменная> <выражение>) …)
;;;;     :outputs  ((<имя> <выражение>) …)
;;;;     :forces   (((pos <слот>) <вектор>) … (:gradient <скаляр> :scale <к>) …)
;;;;     :damping  ((<коэффициент> <линейная скорость>) …)
;;;;     :events   ((<условие> :rising|:falling (set <дискретная> <значение>)) …)
;;;;               ; на границе шага: :rising — при условии > 0, :falling — при < 0
;;;;     :checks   ((<причина> <выражение>) …)  ; сбой шага, если значение < 0
;;;;     :documentation "…")
;;;;
;;;; Вход без :default обязан быть связан в defmodel; вход с :default
;;;; (например, натяжение мышц у рёбер, к которым мышцы не приложены)
;;;; можно не связывать. Проверка компонента — его пробное понижение в IR
;;;; (lower.lisp): так ловятся неизвестные имена, смешение скаляров и
;;;; векторов, скорость узлов вне lagged и :damping — с указанием формы.

(in-package #:worm.dsl)

(defstruct (component (:copier nil))
  name family params state discrete inputs lets derivs outputs forces damping events checks
  documentation source)

(defvar *components* (make-hash-table :test 'eq) "Компоненты по именам (ключевые слова).")

(defun find-component (name &optional (errorp t))
  (or (gethash (kw name) *components*)
      (and errorp (dsl-error 'unknown-component "компонент ~(~A~) не определён" name))))

(defparameter +component-keys+
  '(:family :params :state :discrete :inputs :let :derivs :outputs :forces :damping :events :checks
    :documentation))

(defun check-plist (plist allowed what)
  (unless (evenp (length plist))
    (dsl-error 'dsl-syntax-error "~A: нечётное число элементов в списке ключей" what))
  (loop for (k nil) on plist by #'cddr
        unless (member k allowed)
          do (let ((*form* k))
               (dsl-error 'dsl-syntax-error "~A: неизвестный ключ ~S; допустимы ~{~S~^ ~}" what k allowed))))

(defun pairs (list what)
  "Список (имя выражение) с проверкой формы; имена — ключевые слова."
  (unless (listp list) (dsl-error 'dsl-syntax-error "~A: ожидался список пар" what))
  (mapcar (lambda (p)
            (let ((*form* p))
              (unless (and (consp p) (symbolp (first p)) (first p) (consp (rest p)) (null (cddr p)))
                (dsl-error 'dsl-syntax-error "~A: ожидалось (<имя> <выражение>)" what))
              (list (kw (first p)) (second p))))
          list))

(defun parse-params (list)
  (mapcar (lambda (p)
            (let ((*form* p))
              (cond ((and (symbolp p) p) (list (kw p) :scalar))
                    ((and (consp p) (symbolp (first p)) (equal (rest p) '(:per-element)))
                     (list (kw (first p)) :per-element))
                    (t (dsl-error 'dsl-syntax-error ":params — <имя> или (<имя> :per-element)")))))
          list))

(defun parse-inputs (list)
  (mapcar (lambda (i)
            (let ((*form* i))
              (unless (and (consp i) (symbolp (first i)) (consp (second i))
                           (name-is (first (second i)) "LINK") (keywordp (second (second i)))
                           (null (cddr (second i))))
                (dsl-error 'dsl-syntax-error ":inputs — (<имя> (link <метка>)) с необязательным :default <число>"))
              (destructuring-bind (&key (default nil default-p)) (cddr i)
                (when (and default-p (not (realp default)))
                  (dsl-error 'dsl-syntax-error ":default входа — число"))
                (list (kw (first i)) (second (second i)) (and default-p (float default 0d0))))))
          list))

(defun parse-component (form)
  "Компонент из формы (define-component имя …). Проверяет всё, что можно
проверить без модели."
  (let* ((*component* (and (consp form) (second form)))
         (*form* form))
    (unless (and (consp form) (symbolp (second form)) (second form))
      (dsl-error 'dsl-syntax-error "define-component: ожидалось имя компонента"))
    (let ((plist (cddr form)))
      (check-plist plist +component-keys+ "define-component")
      (destructuring-bind (&key family params state discrete inputs let derivs outputs forces damping events
                                checks documentation) plist
        (unless (member family +families+)
          (let ((*form* family))
            (dsl-error 'family-mismatch "неизвестное семейство ~S; допустимы ~{~(~S~)~^ ~}" family +families+)))
        (let* ((c (make-component
                   :name (second form) :family family
                   :params (parse-params params)
                   :state (pairs state ":state")
                   :discrete (pairs discrete ":discrete")
                   :inputs (parse-inputs inputs)
                   :lets (pairs let ":let")
                   :derivs (pairs derivs ":derivs")
                   :outputs (pairs outputs ":outputs")
                   :forces forces :damping damping :events events
                   :checks (mapcar (lambda (c)
                                     (let ((*form* c))
                                       (unless (and (consp c) (keywordp (first c)) (= (length c) 2))
                                         (dsl-error 'dsl-syntax-error ":checks — (<причина-ключ> <выражение>)"))
                                       (list (first c) (second c))))
                                   checks)
                   :documentation documentation
                   :source form))
               (names (append (mapcar #'first (component-params c)) (mapcar #'first (component-state c))
                              (mapcar #'first (component-discrete c)) (mapcar #'first (component-inputs c))
                              (mapcar #'first (component-lets c)) (mapcar #'first (component-outputs c)))))
          ;; имена не повторяются; производные — ровно у переменных состояния
          (loop for (n . rest) on names
                when (member n rest)
                  do (dsl-error 'dsl-syntax-error "имя ~(~A~) определено в компоненте дважды" n))
          (dolist (d (component-derivs c))
            (unless (assoc (first d) (component-state c))
              (let ((*form* d))
                (dsl-error 'unknown-name "производная задана для ~(~A~), а такой переменной состояния нет" (first d)))))
          (dolist (s (component-state c))
            (unless (assoc (first s) (component-derivs c))
              (let ((*form* s))
                (dsl-error 'dsl-syntax-error "у переменной состояния ~(~A~) нет производной в :derivs" (first s)))))
          (dolist (d (component-discrete c))
            (unless (realp (second d))
              (let ((*form* d)) (dsl-error 'dsl-syntax-error "начальное значение дискретной переменной — число"))))
          (dolist (e events)
            (let ((*form* e))
              (unless (and (consp e) (= (length e) 3) (member (second e) '(:rising :falling)))
                (dsl-error 'dsl-syntax-error ":events — (<выражение> :rising|:falling (set <переменная> <значение>))"))))
          ;; пробное понижение на собственном семействе
          (lower-component c)
          c)))))

(defun register-component (component)
  (setf (gethash (kw (component-name component)) *components*) component)
  (component-name component))

(defmacro define-component (name &body plist)
  "Определить компонент модели. Ошибки — условия model-error с указанием
компонента и формы, сигнализируются при загрузке определения."
  `(register-component (parse-component '(define-component ,name ,@plist))))

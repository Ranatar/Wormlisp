;;;; src/ir/primitives.lisp — примитивы с написанной вручную производной.
;;;;
;;;; Новая функция входит в язык выражений только вместе с производной
;;;; (архитектура, «DSL компонентов и defmodel»):
;;;;
;;;;   (define-primitive name (arg … | &rest args)
;;;;     :indices (j …)        ; необязательно: целые параметры, часть операции
;;;;     :value <функция>      ; (lambda (j … arg …) …) → double-float
;;;;     :derivative <форма>)  ; список частных производных — узлов IR, по
;;;;                           ; одной на аргумент; в форме имена аргументов
;;;;                           ; связаны с узлами, индексов — с числами,
;;;;                           ; SELF — с самим узлом
;;;;
;;;; Значение — функция Лиспа: так задаются величины, которые неудобно или
;;;; дорого выписывать в IR (объём отсека, решение 21). Производная — снова
;;;; узлы IR, часто другие примитивы (градиент объёма, затем гессиан).
;;;; Примитив без :derivative можно вычислять, но не дифференцировать —
;;;; попытка даёт условие no-derivative.

(in-package #:worm.ir)

(defstruct (primitive (:copier nil))
  (name nil :type symbol :read-only t)
  (arity 0 :type fixnum :read-only t)       ; число обязательных аргументов
  (rest-p nil :read-only t)                 ; T — переменное число аргументов
  (n-indices 0 :type fixnum :read-only t)
  (value nil :type function :read-only t)
  (derivative nil :type (or null function) :read-only t)
  (documentation nil :read-only t))

(defvar *primitives* (make-hash-table :test 'eq) "Примитивы по именам.")

(defun find-primitive (name &optional (errorp t))
  (or (gethash name *primitives*)
      (and errorp (error 'ir-type-error :message (format nil "нет примитива ~S" name)))))

(defun register-primitive (name &key arity rest-p n-indices value derivative documentation)
  (setf (gethash name *primitives*)
        (make-primitive :name name :arity arity :rest-p rest-p :n-indices n-indices
                        :value value :derivative derivative :documentation documentation)))

(defmacro define-primitive (name lambda-list &key indices value derivative documentation)
  (let* ((rest-p (eq (first lambda-list) '&rest))
         (args (if rest-p (second lambda-list) lambda-list))
         (self (intern "SELF" (symbol-package name)))
         (idx (gensym "INDICES")) (arg-list (gensym "ARGS")))
    `(register-primitive
      ',name :arity ,(if rest-p 0 (length args)) :rest-p ,rest-p :n-indices ,(length indices)
             :value ,value
             :derivative ,(when derivative
                            `(lambda (,self ,idx ,arg-list)
                               (declare (ignorable ,self))
                               (destructuring-bind ,indices ,idx
                                 (declare (ignorable ,@indices))
                                 ,(if rest-p
                                      `(let ((,args ,arg-list)) ,derivative)
                                      `(destructuring-bind ,args ,arg-list
                                         (declare (ignorable ,@args))
                                         ,derivative)))))
             :documentation ,documentation)))

(defun prim (name args &key indices)
  "Узел примитива NAME от узлов ARGS. Константные аргументы сворачиваются."
  (let ((p (find-primitive name))
        (args (mapcar #'ensure-node args)))
    (unless (if (primitive-rest-p p) t (= (length args) (primitive-arity p)))
      (error 'ir-type-error :message (format nil "примитив ~S ждёт ~D аргументов, а получил ~D"
                                             name (primitive-arity p) (length args))))
    (unless (and (= (length indices) (primitive-n-indices p)) (every #'integerp indices))
      (error 'ir-type-error :message (format nil "примитив ~S ждёт ~D целых индексов, а получил ~S"
                                             name (primitive-n-indices p) indices)))
    (if (every #'const-p args)
        (const (apply (primitive-value p) (append indices (mapcar #'const-value args))))
        (intern-node :prim args (cons name indices)))))

;;; Встроенные примитивы языка выражений

(define-primitive positive (x)
  :value (lambda (x) (max 0d0 x))
  :derivative (list (ir-step x))
  :documentation "Положительная часть max(0, x).")

(define-primitive clamp (x lo hi)
  :value (lambda (x lo hi) (min hi (max lo x)))
  :derivative (list (ir* (ir-step (ir- x lo)) (ir-step (ir- hi x)))
                    (ir-step (ir- lo x))
                    (ir-step (ir- x hi)))
  :documentation "x, ограниченный отрезком [lo, hi].")

(defun hill-value (x k n)
  (if (<= x 0d0) 0d0 (let ((r (expt (/ x k) n))) (/ r (+ 1d0 r)))))

(define-primitive hill-dx (x k n)
  :value (lambda (x k n)
           (if (<= x 0d0) 0d0
               (let ((r (expt (/ x k) n))) (/ (* n r) (* x (sq (+ 1d0 r)))))))
  :documentation "∂hill/∂x = n·rⁿ/(x·(1 + rⁿ)²), r = x/k.")

(define-primitive hill (x k n)
  :value #'hill-value
  :derivative (let ((dx (prim 'hill-dx (list x k n))))
                ;; ∂/∂k = −(x/k)·∂/∂x; ∂/∂n = a·(1 − a)·ln(x/k)
                (list dx
                      (ir- (ir* (ir/ x k) dx))
                      (ir* self (ir- 1d0 self) (ir-log (ir/ x k)))))
  :documentation "Функция Хилла xⁿ/(xⁿ + kⁿ); при x ≤ 0 — ноль.")

(defun logistic (x) (ir/ 1d0 (ir+ 1d0 (ir-exp (ir- x)))))

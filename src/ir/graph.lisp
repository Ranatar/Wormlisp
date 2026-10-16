;;;; src/ir/graph.lisp — узлы графа выражений и конструкторы с упрощением.
;;;;
;;;; Узел — операция, аргументы-узлы и данные: значение константы, ключ
;;;; переменной, показатель степени, имя примитива с индексами. Номер узла —
;;;; порядок создания в графе; аргументы всегда старше узла, поэтому порядок
;;;; номеров — топологический, и обход по нему детерминирован (архитектура,
;;;; «Детерминизм и читаемость»).
;;;;
;;;; Операции: :const :var :+ :* (n-арные) :/ :expt (целая степень) :sqrt
;;;; :exp :log :tanh :sin :cos :step (ступенька Хевисайда, производная 0) :prim.
;;;; Вычитание и отрицание — через умножение на −1.
;;;;
;;;; Упрощение при создании: вложенные суммы и произведения раскрываются,
;;;; константы сворачиваются, нули и единицы убираются, аргументы
;;;; коммутативных операций сортируются по номеру, (expt x 2) становится
;;;; (* x x), деление на константу — умножением.

(in-package #:worm.ir)

(define-condition ir-type-error (codegen-error) ()
  (:documentation "Неверный тип или число аргументов операции IR."))

(define-condition no-derivative (codegen-error)
  ((primitive :initarg :primitive :initform nil :reader no-derivative-primitive))
  (:documentation "Производная нужна, а примитив её не задаёт."))

(defstruct (node (:constructor %make-node (id op args data)) (:copier nil))
  (id 0 :type fixnum :read-only t)
  (op nil :type symbol :read-only t)
  (args #() :type simple-vector :read-only t)
  (data nil :read-only t))

(defmethod print-object ((n node) stream)
  (print-unreadable-object (n stream)
    (format stream "ir ~D ~S" (node-id n) (node-sexp n :depth 3))))

(defstruct (graph (:constructor make-graph ()) (:copier nil))
  (table (make-hash-table :test 'equal) :read-only t)
  (count 0 :type fixnum)
  ;; запоминание производных: (номер узла . номер переменной) → узел
  (derivatives (make-hash-table :test 'equal) :read-only t)
  (dependencies (make-hash-table :test 'eql) :read-only t))

(defvar *graph* nil "Текущий граф: конструкторы создают узлы в нём.")

(defmacro with-graph ((&optional (graph '(make-graph))) &body body)
  `(let ((*graph* ,graph)) ,@body))

(defun graph-node-count (&optional (graph *graph*)) (graph-count graph))

(defun current-graph ()
  (or *graph* (error 'ir-type-error :message "узел IR создаётся вне графа: нужен WITH-GRAPH")))

(defun intern-node (op args data)
  "Найти или создать узел: одинаковые узлы склеиваются."
  (let* ((g (current-graph))
         (key (list* op data (map 'list #'node-id args))))
    (or (gethash key (graph-table g))
        (setf (gethash key (graph-table g))
              (%make-node (prog1 (graph-count g) (incf (graph-count g))) op (coerce args 'simple-vector) data)))))

;;; Листья

(defun const (x)
  (unless (realp x) (error 'ir-type-error :message (format nil "константа IR должна быть числом, а не ~S" x)))
  (intern-node :const #() (float x 0d0)))

(defun var (key) (intern-node :var #() key))

(defun const-p (n) (eq (node-op n) :const))
(defun var-p (n) (eq (node-op n) :var))
(defun const-value (n) (node-data n))
(defun var-key (n) (node-data n))
(defun zero-p (n) (and (const-p n) (= (const-value n) 0d0)))
(defun one-p (n) (and (const-p n) (= (const-value n) 1d0)))

(defun ensure-node (x)
  (cond ((node-p x) x)
        ((realp x) (const x))
        (t (error 'ir-type-error :message (format nil "ожидался скаляр IR, а получено ~S" x)))))

;;; Сумма и произведение

(defun collect-operands (op xs)
  "Аргументы операции OP: вложенные OP раскрываются, константы складываются отдельно."
  (let ((terms '()))
    (dolist (x xs (nreverse terms))
      (let ((n (ensure-node x)))
        (if (eq (node-op n) op)
            (loop for a across (node-args n) do (push a terms))
            (push n terms))))))

(defun sort-by-id (nodes) (sort (copy-list nodes) #'< :key #'node-id))

(defun ir+ (&rest xs)
  (let ((sum 0d0) (rest '()))
    (dolist (n (collect-operands :+ xs))
      (if (const-p n) (incf sum (const-value n)) (push n rest)))
    (setf rest (sort-by-id rest))
    (cond ((null rest) (const sum))
          ((and (null (cdr rest)) (= sum 0d0)) (car rest))
          (t (intern-node :+ (if (= sum 0d0) rest (cons (const sum) rest)) nil)))))

(defun ir* (&rest xs)
  (let ((product 1d0) (rest '()))
    (dolist (n (collect-operands :* xs))
      (if (const-p n) (setf product (* product (const-value n))) (push n rest)))
    (setf rest (sort-by-id rest))
    (cond ((= product 0d0) (const 0d0))
          ((null rest) (const product))
          ((and (null (cdr rest)) (= product 1d0)) (car rest))
          (t (intern-node :* (if (= product 1d0) rest (cons (const product) rest)) nil)))))

(defun ir- (x &rest ys)
  "(ir- x) — отрицание, (ir- x y …) — разность."
  (if (null ys)
      (ir* -1d0 x)
      (apply #'ir+ x (mapcar (lambda (y) (ir* -1d0 y)) ys))))

(defun ir/ (a b)
  (let ((a (ensure-node a)) (b (ensure-node b)))
    (cond ((and (const-p b) (= (const-value b) 0d0))
           (error 'ir-type-error :message "деление на константу ноль в IR"))
          ((const-p b) (ir* a (/ 1d0 (const-value b))))
          ((zero-p a) a)
          ((and (const-p a) (const-p b)) (const (/ (const-value a) (const-value b))))
          (t (intern-node :/ (list a b) nil)))))

(defun ir-expt (x n)
  (unless (integerp n)
    (error 'ir-type-error :message (format nil "expt в языке выражений — только с целой степенью, а не ~S" n)))
  (let ((x (ensure-node x)))
    (cond ((= n 0) (const 1d0))
          ((= n 1) x)
          ((const-p x) (const (expt (const-value x) n)))
          ((= n 2) (ir* x x))
          ((< n 0) (ir/ 1d0 (ir-expt x (- n))))
          (t (intern-node :expt (list x) n)))))

(defun ir-square (x) (ir-expt x 2))

(macrolet ((unary (name op fold)
             `(defun ,name (x)
                (let ((x (ensure-node x)))
                  (if (const-p x)
                      (const (,fold (const-value x)))
                      (intern-node ,op (list x) nil))))))
  (unary ir-exp :exp exp)
  (unary ir-tanh :tanh tanh)
  (unary ir-sin :sin sin)
  (unary ir-cos :cos cos)
  (unary ir-step :step (lambda (v) (if (> v 0d0) 1d0 0d0))))

(defun ir-sqrt (x)
  (let ((x (ensure-node x)))
    (if (const-p x)
        (if (minusp (const-value x))
            (error 'ir-type-error :message "корень из отрицательной константы в IR")
            (const (sqrt (const-value x))))
        (intern-node :sqrt (list x) nil))))

(defun ir-log (x)
  (let ((x (ensure-node x)))
    (if (const-p x)
        (if (plusp (const-value x))
            (const (log (const-value x)))
            (error 'ir-type-error :message "логарифм неположительной константы в IR"))
        (intern-node :log (list x) nil))))

;;; Печать

(defun node-sexp (n &key (depth most-positive-fixnum))
  "Узел как дерево s-выражений (общие подвыражения повторяются)."
  (labels ((walk (n d)
             (cond ((const-p n) (const-value n))
                   ((var-p n) (var-key n))
                   ((<= d 0) '|...|)
                   (t (let ((args (map 'list (lambda (a) (walk a (1- d))) (node-args n))))
                        (case (node-op n)
                          (:expt (list 'expt (first args) (node-data n)))
                          (:prim (list* (car (node-data n)) (append (cdr (node-data n)) args)))
                          (t (cons (intern (symbol-name (node-op n)) :worm.ir) args))))))))
    (walk n depth)))

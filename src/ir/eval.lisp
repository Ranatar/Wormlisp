;;;; src/ir/eval.lisp — интерпретатор IR и печать в виде let*.
;;;;
;;;; Интерпретатор — эталон для кодогенерации (архитектура, «Эталонный
;;;; интерпретатор»): исполняет граф обобщённой арифметикой. Вычислитель
;;;; собирается один раз для набора корней — узлы, достижимые из корней, по
;;;; возрастанию номеров, то есть в топологическом порядке, — и затем
;;;; выполняется для любых значений переменных.

(in-package #:worm.ir)

(defstruct (evaluator (:copier nil))
  (nodes #() :type simple-vector)     ; достижимые узлы по возрастанию номеров
  (ops #() :type simple-vector)       ; операция каждого узла
  (data #() :type simple-vector)      ; данные узла (константа, ключ, степень, примитив)
  (argpos #() :type simple-vector)    ; позиции аргументов в NODES
  (prims #() :type simple-vector)     ; функции значения примитивов
  (roots #() :type simple-vector)     ; позиции корней в NODES
  (values nil :type (simple-array double-float (*))))

(defun reachable (roots)
  (let ((seen (make-hash-table)) (out '()))
    (labels ((walk (n)
               (unless (gethash (node-id n) seen)
                 (setf (gethash (node-id n) seen) t)
                 (loop for a across (node-args n) do (walk a))
                 (push n out))))
      (mapc #'walk roots))
    (sort out #'< :key #'node-id)))

(defun compile-evaluator (roots)
  (let* ((roots (mapcar #'ensure-node roots))
         (nodes (coerce (reachable roots) 'simple-vector))
         (pos (make-hash-table)))
    (loop for n across nodes for i from 0 do (setf (gethash (node-id n) pos) i))
    (make-evaluator
     :nodes nodes
     :ops (map 'simple-vector #'node-op nodes)
     :data (map 'simple-vector #'node-data nodes)
     :argpos (map 'simple-vector (lambda (n) (map 'simple-vector (lambda (a) (gethash (node-id a) pos))
                                                  (node-args n)))
                  nodes)
     :prims (map 'simple-vector (lambda (n) (when (eq (node-op n) :prim)
                                              (primitive-value (find-primitive (car (node-data n))))))
                 nodes)
     :roots (map 'simple-vector (lambda (r) (gethash (node-id r) pos)) roots)
     :values (make-array (length nodes) :element-type 'double-float))))

(defun evaluator-variables (ev)
  "Ключи переменных, которые читает вычислитель, по порядку узлов."
  (loop for op across (evaluator-ops ev) for d across (evaluator-data ev)
        when (eq op :var) collect d))

(defun run-evaluator (ev env &optional out)
  "Вычислить корни; ENV — функция от ключа переменной к числу. Значения
корней — в вектор OUT, если он задан, иначе списком."
  (let ((ops (evaluator-ops ev)) (data (evaluator-data ev)) (argpos (evaluator-argpos ev))
        (prims (evaluator-prims ev)) (vals (evaluator-values ev)))
    (dotimes (i (length ops))
      (let ((ap (aref argpos i)))
        (flet ((arg (k) (aref vals (aref ap k))))
          (setf (aref vals i)
                (float
                 (ecase (aref ops i)
                   (:const (aref data i))
                   (:var (funcall env (aref data i)))
                   (:+ (let ((s 0d0)) (dotimes (k (length ap) s) (incf s (arg k)))))
                   (:* (let ((p 1d0)) (dotimes (k (length ap) p) (setf p (* p (arg k))))))
                   (:/ (/ (arg 0) (arg 1)))
                   (:expt (expt (arg 0) (aref data i)))
                   (:sqrt (let ((x (arg 0)))
                            (if (minusp x)
                                (error 'floating-point-invalid-operation :operation 'sqrt :operands (list x))
                                (sqrt x))))
                   (:exp (exp (arg 0)))
                   (:log (let ((x (arg 0)))
                           (if (plusp x) (log x)
                               (error 'floating-point-invalid-operation :operation 'log :operands (list x)))))
                   (:tanh (tanh (arg 0)))
                   (:sin (sin (arg 0)))
                   (:cos (cos (arg 0)))
                   (:step (if (> (arg 0) 0d0) 1d0 0d0))
                   (:prim (apply (aref prims i)
                                 (append (cdr (aref data i))
                                         (loop for k below (length ap) collect (arg k))))))
                 0d0)))))
    (let ((roots (evaluator-roots ev)))
      (if out
          (dotimes (k (length roots) out) (setf (aref out k) (aref vals (aref roots k))))
          (map 'list (lambda (i) (aref vals i)) roots)))))

(defun evaluate (roots env)
  "Значение узла (или списка узлов) при значениях переменных из ENV:
функции, хэш-таблицы или списка ((ключ . значение) …)."
  (let ((fn (etypecase env
              (function env)
              (hash-table (lambda (k) (multiple-value-bind (v found) (gethash k env)
                                        (if found v (error 'ir-type-error :message (format nil "переменная ~S не задана" k))))))
              (list (lambda (k) (let ((cell (assoc k env :test #'equal)))
                                  (if cell (cdr cell) (error 'ir-type-error :message (format nil "переменная ~S не задана" k))))))))
        (single (node-p roots)))
    (let ((vals (run-evaluator (compile-evaluator (if single (list roots) roots)) fn)))
      (if single (first vals) vals))))

(defun let-form (roots &key (var-name (lambda (key) key)))
  "Корни как (let* ((vN …) …) (values …)): общие подвыражения названы
один раз, порядок — по номерам узлов. VAR-NAME превращает ключ переменной
в форму её чтения."
  (let* ((roots (mapcar #'ensure-node roots))
         (nodes (reachable roots))
         (names (make-hash-table)))
    (flet ((ref (n)
             (cond ((const-p n) (const-value n))
                   ((var-p n) (funcall var-name (var-key n)))
                   (t (gethash (node-id n) names)))))
      (let ((bindings
              (loop for n in nodes
                    unless (or (const-p n) (var-p n))
                      collect (let ((sym (intern (format nil "V~D" (node-id n)) :worm.ir))
                                    (args (map 'list #'ref (node-args n))))
                                (setf (gethash (node-id n) names) sym)
                                (list sym
                                      (case (node-op n)
                                        (:expt (list 'expt (first args) (node-data n)))
                                        (:step (list 'step (first args)))
                                        (:prim (list* (car (node-data n)) (append (cdr (node-data n)) args)))
                                        (t (cons (intern (symbol-name (node-op n)) :worm.ir) args))))))))
        `(let* ,bindings (values ,@(mapcar #'ref roots)))))))

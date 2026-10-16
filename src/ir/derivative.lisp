;;;; src/ir/derivative.lisp — символьная производная.
;;;;
;;;; (derivative f x) — узел ∂f/∂x, где x — переменная. Результат
;;;; запоминается в графе по паре номеров, поэтому повторные и общие
;;;; производные не пересчитываются, а склейка узлов делает их общими
;;;; подвыражениями. Ветви, не зависящие от x, сразу дают ноль.

(in-package #:worm.ir)

(defun node-vars (n)
  "Номера переменных, от которых зависит N, по возрастанию (запоминается)."
  (let ((memo (graph-dependencies (current-graph))))
    (labels ((walk (n)
               (multiple-value-bind (v found) (gethash (node-id n) memo)
                 (if found v
                     (setf (gethash (node-id n) memo)
                           (cond ((var-p n) (list (node-id n)))
                                 ((const-p n) '())
                                 (t (let ((acc '()))
                                      (loop for a across (node-args n)
                                            do (setf acc (union acc (walk a))))
                                      ;; union может вернуть список ребёнка: сортировать копию
                                      (sort (copy-list acc) #'<)))))))))
      (walk n))))

(defun depends-on-p (n x)
  (member (node-id (ensure-var x)) (node-vars n)))

(defun ensure-var (x)
  (cond ((and (node-p x) (var-p x)) x)
        ((node-p x) (error 'ir-type-error :message (format nil "дифференцировать можно только по переменной, а не по ~S" (node-sexp x :depth 2))))
        (t (var x))))

(defun derivative (f x)
  "∂F/∂X; X — узел-переменная или её ключ."
  (let* ((x (ensure-var x))
         (f (ensure-node f))
         (memo (graph-derivatives (current-graph))))
    (labels ((d (n)
               (let ((key (cons (node-id n) (node-id x))))
                 (or (gethash key memo)
                     (setf (gethash key memo)
                           (cond ((eq n x) (const 1d0))
                                 ((not (member (node-id x) (node-vars n))) (const 0d0))
                                 (t (rule n)))))))
             (rule (n)
               (let ((args (node-args n)))
                 (ecase (node-op n)
                   (:+ (apply #'ir+ (map 'list #'d args)))
                   (:* (apply #'ir+
                              (loop for i below (length args)
                                    collect (apply #'ir* (d (aref args i))
                                                   (loop for j below (length args)
                                                         unless (= i j) collect (aref args j))))))
                   (:/ (let ((a (aref args 0)) (b (aref args 1)))
                         (ir- (ir/ (d a) b) (ir/ (ir* a (d b)) (ir* b b)))))
                   (:expt (let ((m (node-data n)) (a (aref args 0)))
                            (ir* m (ir-expt a (1- m)) (d a))))
                   (:sqrt (ir/ (d (aref args 0)) (ir* 2d0 n)))
                   (:exp (ir* n (d (aref args 0))))
                   (:log (ir/ (d (aref args 0)) (aref args 0)))
                   (:tanh (ir* (ir- 1d0 (ir* n n)) (d (aref args 0))))
                   (:sin (ir* (ir-cos (aref args 0)) (d (aref args 0))))
                   (:cos (ir- (ir* (ir-sin (aref args 0)) (d (aref args 0)))))
                   (:step (const 0d0))
                   (:prim (let* ((name (car (node-data n)))
                                 (p (find-primitive name))
                                 (fn (primitive-derivative p)))
                            (unless fn
                              (error 'no-derivative :primitive name
                                                    :message (format nil "у примитива ~S нет производной" name)))
                            (let ((partials (funcall fn n (cdr (node-data n)) (coerce args 'list))))
                              (unless (= (length partials) (length args))
                                (error 'ir-type-error
                                       :message (format nil "производная примитива ~S дала ~D частных вместо ~D"
                                                        name (length partials) (length args))))
                              (apply #'ir+ (loop for a across args for pd in partials
                                                 unless (zero-p (d a))
                                                   collect (ir* (ensure-node pd) (d a)))))))))))
      (d f))))

(defun gradient (f xs)
  "Список ∂F/∂x для каждого x из XS."
  (mapcar (lambda (x) (derivative f x)) xs))

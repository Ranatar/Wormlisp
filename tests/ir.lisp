;;;; tests/ir.lisp — промежуточное представление выражений (этап 6).
;;;;
;;;; Условие готовности: производные IR совпадают с конечными разностями на
;;;; случайных точках для всех операций языка выражений.

(in-package #:worm.tests)

(def-suite ir :in worm :description "worm.ir.")
(in-suite ir)

(defmacro with-ir (&body body)
  `(worm.ir:with-graph () ,@body))

(test equal-nodes-are-shared
  (with-ir
    (let ((x (worm.ir:var :x)) (y (worm.ir:var :y)))
      (is (eq (worm.ir:var :x) x) "переменная одна")
      (is (eq (worm.ir:ir+ x y) (worm.ir:ir+ y x)) "сумма коммутативна: один узел")
      (is (eq (worm.ir:ir* x (worm.ir:ir+ x y)) (worm.ir:ir* (worm.ir:ir+ y x) x)))
      (is (eq (worm.ir:ir-exp (worm.ir:ir* 2 x)) (worm.ir:ir-exp (worm.ir:ir* x 2))))
      (let ((before (worm.ir:graph-node-count)))
        (worm.ir:ir-sqrt (worm.ir:ir+ (worm.ir:ir* x x) (worm.ir:ir* y y)))
        (worm.ir:ir-sqrt (worm.ir:ir+ (worm.ir:ir* y y) (worm.ir:ir* x x)))
        (is (= (+ before 4) (worm.ir:graph-node-count)) "второй раз новых узлов нет")))))

(test construction-simplifies
  (with-ir
    (let ((x (worm.ir:var :x)))
      (is (eq x (worm.ir:ir+ x 0)))
      (is (eq x (worm.ir:ir* x 1)))
      (is (worm.ir:zero-p (worm.ir:ir* x 0)))
      (is (= 7d0 (worm.ir:const-value (worm.ir:ir+ 3 4))))
      (is (equal '(* :x :x) (worm.ir:node-sexp (worm.ir:ir-expt x 2))) "x² — умножение")
      (is (equal '(* 0.5d0 :x) (worm.ir:node-sexp (worm.ir:ir/ x 2))) "деление на константу — умножение")
      (is (eq x (worm.ir:ir-expt x 1)))
      (is (= 1d0 (worm.ir:const-value (worm.ir:ir-expt x 0))))
      (is (= 2d0 (worm.ir:const-value (worm.ir:ir-sqrt 4))) "константы сворачиваются и в функциях")
      (is (= 0.5d0 (worm.ir:const-value (worm.ir:prim 'worm.ir:hill '(1 1 3)))) "и в примитивах"))))

(test let-form-names-shared-subexpressions-once
  (with-ir
    (let* ((x (worm.ir:var :x))
           (s (worm.ir:ir-sqrt (worm.ir:ir+ (worm.ir:ir* x x) 1)))
           (form (worm.ir:let-form (list (worm.ir:ir* s s) (worm.ir:ir+ s 1)))))
      (is (eq 'let* (first form)))
      (is (= 1 (count-if (lambda (b) (and (consp (second b)) (eq 'sqrt (first (second b))))) (second form)))
          "корень вычисляется один раз"))))

;;; Производные против конечных разностей

(defun fd-check (expr vars point &key (eps 1d-6))
  "Наибольшее относительное расхождение символьной производной EXPR по VARS
с центральной разностью в точке POINT (список значений)."
  (let ((worst 0d0))
    (loop for v in vars for i from 0
          do (let* ((d (worm.ir:derivative expr v))
                    (env (mapcar #'cons (mapcar #'worm.ir:var-key vars) point))
                    (an (worm.ir:evaluate d env))
                    (h (* eps (max 1d0 (abs (nth i point)))))
                    (plus (copy-list point)) (minus (copy-list point)))
               (incf (nth i plus) h) (decf (nth i minus) h)
               (let ((fd (/ (- (worm.ir:evaluate expr (mapcar #'cons (mapcar #'worm.ir:var-key vars) plus))
                               (worm.ir:evaluate expr (mapcar #'cons (mapcar #'worm.ir:var-key vars) minus)))
                            (* 2 h))))
                 (setf worst (max worst (/ (abs (- fd an)) (max 1d0 (abs fd))))))))
    worst))

(defparameter +expression-cases+
  ;; (имя построитель область) — область: функция случайного значения для каждой переменной
  `((:sum ,(lambda (x y z) (worm.ir:ir+ x y z 3)))
    (:difference ,(lambda (x y z) (worm.ir:ir- x y z)))
    (:product ,(lambda (x y z) (worm.ir:ir* x y z 2)))
    (:quotient ,(lambda (x y z) (worm.ir:ir/ (worm.ir:ir+ x y) (worm.ir:ir+ z 3))))
    (:cube ,(lambda (x y z) (declare (ignore y z)) (worm.ir:ir-expt x 3)))
    (:inverse-square ,(lambda (x y z) (declare (ignore y)) (worm.ir:ir-expt (worm.ir:ir+ x z 3) -2)))
    (:square ,(lambda (x y z) (declare (ignore z)) (worm.ir:ir-square (worm.ir:ir- x y))))
    (:sqrt ,(lambda (x y z) (worm.ir:ir-sqrt (worm.ir:ir+ (worm.ir:ir* x x) (worm.ir:ir* y y) (worm.ir:ir* z z) 0.1d0))))
    (:exp ,(lambda (x y z) (declare (ignore z)) (worm.ir:ir-exp (worm.ir:ir* x y))))
    (:log ,(lambda (x y z) (declare (ignore y)) (worm.ir:ir-log (worm.ir:ir+ (worm.ir:ir* x x) z 3))))
    (:tanh ,(lambda (x y z) (worm.ir:ir-tanh (worm.ir:ir+ x (worm.ir:ir* y z)))))
    (:sin ,(lambda (x y z) (worm.ir:ir-sin (worm.ir:ir+ x (worm.ir:ir* 2 y z)))))
    (:cos ,(lambda (x y z) (declare (ignore z)) (worm.ir:ir-cos (worm.ir:ir* x y))))
    (:logistic ,(lambda (x y z) (declare (ignore z)) (worm.ir:logistic (worm.ir:ir* 3 x y))))
    (:positive ,(lambda (x y z) (worm.ir:prim 'worm.ir:positive (list (worm.ir:ir+ x y z)))))
    (:clamp ,(lambda (x y z) (worm.ir:prim 'worm.ir:clamp (list x (worm.ir:ir- y 2) (worm.ir:ir+ z 2)))))
    (:hill ,(lambda (x y z) (worm.ir:prim 'worm.ir:hill (list (worm.ir:ir+ (worm.ir:ir* x x) 0.1d0)
                                                             (worm.ir:ir+ (worm.ir:ir* y y) 0.2d0)
                                                             (worm.ir:ir+ z 3)))))
    (:composition ,(lambda (x y z)
                   (let ((u (worm.ir:ir-sqrt (worm.ir:ir+ (worm.ir:ir* x x) (worm.ir:ir* y y) 1))))
                     (worm.ir:ir* (worm.ir:ir/ x u) (worm.ir:ir-tanh (worm.ir:ir* u z))
                                  (worm.ir:prim 'worm.ir:hill (list u 1.5d0 3))))))))

(test derivatives-match-finite-differences
  (let ((state (sb-ext:seed-random-state 61)))
    (dolist (c +expression-cases+)
      (destructuring-bind (name build) c
        (with-ir
          (let* ((vars (list (worm.ir:var :x) (worm.ir:var :y) (worm.ir:var :z)))
                 (expr (apply build vars))
                 (worst 0d0))
            (dotimes (k 40)
              (let ((point (loop repeat 3 collect (- (random 4d0 state) 2d0))))
                ;; разрывные места (ступеньки) обходятся стороной
                (unless (and (member name '(:positive :clamp))
                             (< (min (abs (reduce #'+ point)) (abs (- (first point) (- (second point) 2)))
                                     (abs (- (first point) (+ (third point) 2))))
                                1d-3))
                  (setf worst (max worst (fd-check expr vars point))))))
            (is (< worst 1d-6) "~(~A~): расхождение ~,2E" name worst)))))))

(test second-derivatives-where-defined
  (with-ir
    (let* ((x (worm.ir:var :x)) (y (worm.ir:var :y))
           (f (worm.ir:ir* (worm.ir:ir-exp (worm.ir:ir* x y)) (worm.ir:ir-tanh x)))
           (dx (worm.ir:derivative f x)))
      (is (< (fd-check dx (list x y) '(0.3d0 -0.7d0)) 1d-6) "∂²f по x и y"))))

(test missing-derivative-is-a-condition
  (with-ir
    (let* ((x (worm.ir:var :x))
           (h (worm.ir:prim 'worm.ir:hill (list x 0.5d0 3)))
           (dh (worm.ir:derivative h x)))
      (is (< (fd-check h (list x) '(0.7d0)) 1d-7) "первая производная Хилла есть")
      (handler-case (progn (worm.ir:derivative dh x) (fail "вторая производная не должна строиться"))
        (worm.ir:no-derivative (e)
          (is (eq 'worm.ir:hill-dx (worm.ir:no-derivative-primitive e)) "условие называет примитив"))))))

(test ir-type-errors
  (with-ir
    (let ((x (worm.ir:var :x)))
      (signals worm.ir:ir-type-error (worm.ir:ir-expt x 0.5d0))
      (signals worm.ir:ir-type-error (worm.ir:prim 'worm.ir:hill (list x 1)))
      (signals worm.ir:ir-type-error (worm.ir:prim 'no-such-primitive (list x)))
      (signals worm.ir:ir-type-error (worm.ir:ir+ x "строка"))
      (signals worm.ir:ir-type-error (worm.ir:ir/ x 0))
      (signals worm.ir:ir-type-error (worm.ir:derivative x (worm.ir:ir+ x 1)))
      (signals worm.ir:ir-type-error (worm.ir:evaluate (worm.ir:ir+ x (worm.ir:var :y)) '((:x . 1d0))))))
  (signals worm.ir:ir-type-error (worm.ir:var :outside-a-graph)))

(test evaluator-runs-many-times
  (with-ir
    (let* ((x (worm.ir:var :x)) (y (worm.ir:var :y))
           (ev (worm.ir:compile-evaluator (list (worm.ir:ir* x y) (worm.ir:ir+ x y)))))
      (is (equal '(:x :y) (worm.ir:evaluator-variables ev)))
      (is (equal '(6d0 5d0) (worm.ir:run-evaluator ev (lambda (k) (if (eq k :x) 2d0 3d0)))))
      (is (equal '(-4d0 3d0) (worm.ir:run-evaluator ev (lambda (k) (if (eq k :x) -1d0 4d0))))))))

(worm.ir:define-primitive test-cube-root (x)
  :value (lambda (x) (if (minusp x) (- (expt (- x) 1/3)) (expt x 1/3)))
  :derivative (list (worm.ir:ir/ 1d0 (worm.ir:ir* 3 self self)))
  :documentation "Кубический корень: пример примитива, производная через SELF.")

(test user-primitive-with-derivative
  (with-ir
    (let* ((x (worm.ir:var :x))
           (f (worm.ir:prim 'test-cube-root (list (worm.ir:ir+ (worm.ir:ir* x x) 1)))))
      (is (worm.core:approx= 2d0 (worm.ir:evaluate f '((:x . 2.6457513110645907d0))) :rel 1d-12))
      (is (< (fd-check f (list x) '(0.8d0)) 1d-7)))))

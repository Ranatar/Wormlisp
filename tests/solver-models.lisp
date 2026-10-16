;;;; tests/solver-models.lisp — эталонные модели этапа 2, написанные руками.
;;;;
;;;; Решатель проверяется на них до появления тела, чтобы ошибки численного
;;;; метода не смешивались с ошибками физики (план, этап 2). Каждая модель —
;;;; compiled-model с функциями по соглашениям worm.core/model.lisp:
;;;; буферы результатов обнулены решателем, функции только прибавляют.

(in-package #:worm.tests)

;;; 1. Цепочка вязкоупругих звеньев.
;;;
;;; n узлов с одной координатой, концы цепочки закреплены. Каждый узел
;;; тянет среда с коэффициентом c, соседние узлы связаны пружиной k и
;;; демпфером η параллельно:
;;;   (c·I + η·L)·ẏ = −k·L·y,  L — матрица Лапласа цепочки (2 на диагонали, −1 рядом).
;;; Собственные векторы L — sin(jπ(i+1)/(n+1)), числа λⱼ = 2 − 2cos(jπ/(n+1)),
;;; поэтому мода j затухает точно как exp(−rⱼ·t), rⱼ = k·λⱼ/(c + η·λⱼ).

(defun chain-model (n)
  (flet ((laplacian-band (ab scale-diag scale-off)
           (declare (type worm.core:dvec ab) (type double-float scale-diag scale-off))
           (dotimes (i n)
             (incf (aref ab (worm.core:band-index i i 1 1)) scale-diag)
             (when (> i 0) (incf (aref ab (worm.core:band-index i (1- i) 1 1)) scale-off))
             (when (< i (1- n)) (incf (aref ab (worm.core:band-index i (1+ i) 1 1)) scale-off)))))
    (declare (inline laplacian-band))
    (worm.core:make-compiled-model
     :name 'chain :n-mech n :lower-band 1 :upper-band 1
     :mech-forces
     (lambda (y syn p g)
       (declare (type worm.core:dvec y syn p g) (ignore syn) (optimize speed))
       (let ((k (aref p 2)))
         (dotimes (i n)
           (let ((left (if (> i 0) (aref y (1- i)) 0d0))
                 (right (if (< i (1- n)) (aref y (1+ i)) 0d0)))
             (incf (aref g i) (* (- k) (- (* 2d0 (aref y i)) left right))))))
       (values))
     :mech-matrix
     (lambda (y syn p clock ab)
       (declare (type worm.core:dvec y syn p clock ab) (ignore y syn) (optimize speed))
       (let ((c (aref p 0)) (eta (aref p 1)) (k (aref p 2))
             (h (aref clock worm.core:+clock-h+)))
         ;; B − h·∂G/∂y = c·I + (η + h·k)·L
         (dotimes (i n) (incf (aref ab (worm.core:band-index i i 1 1)) c))
         (laplacian-band ab (* 2d0 (+ eta (* h k))) (- (+ eta (* h k)))))
       (values)))))

(defun chain-mode (n j i)
  (sin (/ (* j pi (1+ i)) (1+ n))))

(defun chain-rate (n j c eta k)
  (let ((lam (- 2d0 (* 2d0 (cos (/ (* j pi) (1+ n)))))))
    (/ (* k lam) (+ c (* eta lam)))))

(defparameter *chain-amplitudes* '((1 . 1d0) (2 . -0.5d0) (5 . 0.25d0)))

(defun chain-exact (n time c eta k)
  "Точное решение цепочки из суммы трёх мод *CHAIN-AMPLITUDES*."
  (let ((y (worm.core:make-dvec n)))
    (dotimes (i n y)
      (setf (aref y i)
            (loop for (j . a) in *chain-amplitudes*
                  sum (* a (chain-mode n j i) (exp (- (* (chain-rate n j c eta k) time)))))))))

(defun max-difference (a b)
  (loop for x across a for y across b maximize (abs (- x y))))

;;; 2. Жёсткая линейная система: быстрая мода λ₁ = 10⁶ подчинена медленной.
;;;   ẏ₁ = −λ₁·(y₁ − y₂),  ẏ₂ = −y₂,  y(0) = (1, 1);
;;;   y₂ = e^{−t},  y₁ = −e^{−λ₁t}/(λ₁ − 1) + λ₁·e^{−t}/(λ₁ − 1).

(defun stiff-model ()
  (worm.core:make-compiled-model
   :name 'stiff :n-mech 2 :lower-band 1 :upper-band 1
   :mech-forces
   (lambda (y syn p g)
     (declare (type worm.core:dvec y syn p g) (ignore syn) (optimize speed))
     (let ((l1 (aref p 0)))
       (incf (aref g 0) (* (- l1) (- (aref y 0) (aref y 1))))
       (incf (aref g 1) (- (aref y 1))))
     (values))
   :mech-matrix
   (lambda (y syn p clock ab)
     (declare (type worm.core:dvec y syn p clock ab) (ignore y syn) (optimize speed))
     (let ((l1 (aref p 0)) (h (aref clock worm.core:+clock-h+)))
       (incf (aref ab (worm.core:band-index 0 0 1 1)) (+ 1d0 (* h l1)))
       (incf (aref ab (worm.core:band-index 0 1 1 1)) (- (* h l1)))
       (incf (aref ab (worm.core:band-index 1 1 1 1)) (+ 1d0 h)))
     (values))))

(defun stiff-exact (time l1)
  (worm.core:dvec-from
   (list (+ (/ (- (exp (- (* l1 time)))) (- l1 1)) (/ (* l1 (exp (- time))) (- l1 1)))
         (exp (- time)))))

;;; 3. Модели со сбоями.

(defun decay-model (&key nan-at-call trap-above-h)
  "ẏ = −y для одной переменной. NAN-AT-CALL — номер вызова mech-forces, на
котором в силу один раз подставляется NaN (без ловушки: так NaN приходит,
например, из данных). TRAP-ABOVE-H — при h больше этого значения
mech-matrix делит ноль на ноль, и срабатывает ловушка недопустимой операции."
  (let ((calls 0))
    (worm.core:make-compiled-model
     :name 'decay :n-mech 1
     :mech-forces
     (lambda (y syn p g)
       (declare (type worm.core:dvec y syn p g) (ignore syn p))
       (incf calls)
       (incf (aref g 0)
             (if (and nan-at-call (= calls nan-at-call))
                 (sb-kernel:make-double-float -524288 0) ; тихий NaN из битов, без ловушки
                 (- (aref y 0))))
       (values))
     :mech-matrix
     (lambda (y syn p clock ab)
       (declare (type worm.core:dvec y syn p clock ab) (ignore y syn p))
       (let ((h (aref clock worm.core:+clock-h+)))
         (incf (aref ab 0) (+ 1d0 h))
         (when (and trap-above-h (> h trap-above-h))
           ;; 0/0 во время работы: ловушка недопустимой операции
           (incf (aref ab 0) (let ((zero (- h h))) (/ zero zero)))))
       (values)))))

;;; 4. Связанная модель: нервы уровня 3 и механика.
;;;   нервы:    ẋ = −a·x − x³          (нелинейна, якобиан −a − 3x²)
;;;   выход:    syn = x
;;;   механика: ẏ = −b·y + syn
;;; Плюс наблюдаемая obs = y и дискретное z = [obs > порог] — чтобы
;;; проверить порядок вызовов шага. x(t) известен точно:
;;; 1/x² = (1/x₀² + 1/a)·e^{2at} − 1/a.

(defun coupled-model ()
  (worm.core:make-compiled-model
   :name 'coupled :n-mech 1 :n-neural 1 :n-discrete 1 :n-observables 1 :n-syn 1
   :observables
   (lambda (y aux p obs)
     (declare (type worm.core:dvec y aux p obs) (ignore aux p))
     (incf (aref obs 0) (aref y 0))
     (values))
   :neural-discrete
   (lambda (obs z p clock)
     (declare (type worm.core:dvec obs z p clock) (ignore clock))
     (setf (aref z 0) (if (> (aref obs 0) (aref p 2)) 1d0 0d0))
     (values))
   :neural-rhs
   (lambda (x obs z p clock f)
     (declare (type worm.core:dvec x obs z p clock f) (ignore obs z clock))
     (let ((v (aref x 0)))
       (incf (aref f 0) (- (- (* (aref p 0) v)) (* v v v))))
     (values))
   :neural-jacobian
   (lambda (x obs z p clock jac)
     (declare (type worm.core:dvec x obs z p clock) (type worm.core:dmat jac) (ignore obs z clock))
     (let ((v (aref x 0)))
       (incf (aref jac 0 0) (- (- (aref p 0)) (* 3d0 v v))))
     (values))
   :neural-output
   (lambda (x obs z p clock syn)
     (declare (type worm.core:dvec x obs z p clock syn) (ignore obs z p clock))
     (incf (aref syn 0) (aref x 0))
     (values))
   :mech-forces
   (lambda (y syn p g)
     (declare (type worm.core:dvec y syn p g))
     (incf (aref g 0) (+ (- (* (aref p 1) (aref y 0))) (aref syn 0)))
     (values))
   :mech-matrix
   (lambda (y syn p clock ab)
     (declare (type worm.core:dvec y syn p clock ab) (ignore y syn))
     (incf (aref ab 0) (+ 1d0 (* (aref clock worm.core:+clock-h+) (aref p 1))))
     (values))))

(defun coupled-x-exact (time a x0)
  (/ 1d0 (sqrt (- (* (+ (/ 1d0 (* x0 x0)) (/ 1d0 a)) (exp (* 2 a time))) (/ 1d0 a)))))

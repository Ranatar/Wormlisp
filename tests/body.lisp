;;;; tests/body.lisp — ручные ядра тела и среды: быстрые проверки.
;;;;
;;;; Условие готовности этапа 3: якобиан каждого ручного ядра совпадает с
;;;; конечными разностями. Якобиан берётся из самой матрицы шага:
;;;; A(h) = B − h·J, поэтому J = (A(h₁) − A(h₂))/(h₂ − h₁) — так
;;;; проверяется и сборка в ленту, а не только формулы ядра.
;;;; Длинные физические тесты — в tests/physics.lisp (make test-full).

(in-package #:worm.tests)

(def-suite body :in worm :description "Ручные ядра тела и среды.")
(in-suite body)

(defun band-to-dense (ab n kl ku)
  (let ((m (worm.core:make-dmat n n)))
    (dotimes (j n m)
      (loop for i from (max 0 (- j ku)) to (min (1- n) (+ j kl))
            do (setf (aref m i j) (aref ab (worm.core:band-index i j kl ku)))))))

(defun kernel-jacobian-error (params kernels &key (seed 7) (rings 8) prestress)
  "Наибольшее расхождение якобиана модели с конечными разностями сил,
делённое на наибольший элемент якобиана."
  (let* ((body (worm.oracle:build-body :params params :kernels kernels :rings rings
                                       :prestress prestress))
         (model (worm.oracle:body-model body))
         (n (worm.core:compiled-model-n-mech model))
         (kl (worm.core:compiled-model-lower-band model))
         (state (sb-ext:seed-random-state seed))
         (y (copy-seq (worm.oracle:body-y0 body)))
         (p (worm.oracle:body-pvec body))
         (syn (worm.core:make-dvec 95)))
    (dotimes (i n) (incf (aref y i) (* 0.004d0 (- (random 1d0 state) 0.5d0))))
    (dotimes (m 95) (setf (aref syn m) (random 20d0 state)))
    (flet ((matrix (h)
             (let ((ab (worm.core:make-band n kl kl)))
               (funcall (worm.core:compiled-model-mech-matrix model) y (worm.core:make-dvec 0) syn p
                        (worm.core:dvec-from (list 0 h 0)) ab)
               (band-to-dense ab n kl kl)))
           (forces (yy)
             (let ((g (worm.core:make-dvec n)))
               (funcall (worm.core:compiled-model-mech-forces model) yy syn p g)
               g)))
      (let ((a1 (matrix 1d-3)) (a2 (matrix 2d-3)) (worst 0d0) (scale 0d0) (eps 1d-7))
        (dotimes (k n)
          (let ((yp (copy-seq y)) (ym (copy-seq y)))
            (incf (aref yp k) eps) (decf (aref ym k) eps)
            (let ((gp (forces yp)) (gm (forces ym)))
              (dotimes (i n)
                (let ((fd (/ (- (aref gp i) (aref gm i)) (* 2 eps)))
                      (an (/ (- (aref a1 i k) (aref a2 i k)) 1d-3)))
                  (setf scale (max scale (abs fd))
                        worst (max worst (abs (- fd an)))))))))
        (/ worst scale)))))

(test each-kernel-jacobian-matches-finite-differences
  (loop for (params kernels) in '(("water-v1" (:long-edge))
                                  ("water-v1" (:long-edge :tension))
                                  ("water-v1" (:circ-edge))
                                  ("water-v1" (:diagonal))
                                  ("water-v1" (:diametral))
                                  ("water-v1" (:pressure))
                                  ("agar-a-v1" (:contact))
                                  ("agar-a-v1" (:long-edge :circ-edge :diagonal :diametral
                                                :pressure :drag :contact :tension)))
        do (let ((err (kernel-jacobian-error params kernels)))
             (is (< err 1d-7) "~A ~(~S~): относительное расхождение ~,2E" params kernels err)))
  ;; с предварительным давлением 20 кПа: натяжения покоя и p₀ в давлении
  (loop for kernels in '((:circ-edge) (:long-edge) (:diagonal) (:pressure)
                         (:long-edge :circ-edge :diagonal :diametral :pressure :drag :tension))
        do (let ((err (kernel-jacobian-error "water-v1" kernels :prestress 2d4)))
             (is (< err 1d-7) "p₀ = 20 кПа, ~(~S~): относительное расхождение ~,2E" kernels err))))

(test prestressed-rest-state-is-in-equilibrium
  ;; Прямое тело покоя с p₀ и натяжениями рёбер покоя не испытывает сил:
  ;; осевые гасят продольные рёбра, радиальные — окружные, касательные на
  ;; конических концах — диагонали двух закруток.
  (multiple-value-bind (circ long diag residual)
      (worm.oracle:prestress-tensions (worm.topology:make-tube :rings 50 :ring-nodes 8))
    (declare (ignore diag))
    (is (< residual 1d-12) "невязка ~,2E" residual)
    (is (every #'plusp circ) "окружные рёбра растянуты")
    (is (every #'plusp long) "продольные рёбра растянуты"))
  (let* ((body (worm.oracle:build-body :params "water-v1" :prestress 3d4))
         (model (worm.oracle:body-model body))
         (y (worm.oracle:body-y0 body))
         (g (worm.core:make-dvec (length y))))
    (funcall (worm.core:compiled-model-mech-forces model) y (worm.core:make-dvec 95)
             (worm.oracle:body-pvec body) g)
    (is (< (reduce #'max g :key #'abs) 1d-9) "сила покоя при 30 кПа: ~,2E мкН" (reduce #'max g :key #'abs))))

(test drag-and-viscosity-go-to-b-not-to-the-jacobian
  ;; Сопротивление и вязкость линейны по скоростям и входят в B, а не в J:
  ;; матрица при h = 0 — это B, она симметрична и положительна на диагонали.
  (let* ((body (worm.oracle:build-body :params "agar-a-v1" :rings 6))
         (model (worm.oracle:body-model body))
         (n (worm.core:compiled-model-n-mech model))
         (kl (worm.core:compiled-model-lower-band model))
         (ab (worm.core:make-band n kl kl)))
    (funcall (worm.core:compiled-model-mech-matrix model) (worm.oracle:body-y0 body) (worm.core:make-dvec 0)
             (worm.core:make-dvec 95) (worm.oracle:body-pvec body) (worm.core:dvec-from '(0 0 0)) ab)
    (let ((b (band-to-dense ab n kl kl)))
      (is (loop for i below n always (plusp (aref b i i))))
      (is (loop for i below n always (loop for j below n always (worm.core:approx= (aref b i j) (aref b j i) :rel 1d-12 :abs 1d-14)))))))

(test volume-gradient-and-hessian-match-finite-differences
  (let* ((tube (worm.topology:make-tube :rings 4 :ring-nodes 8))
         (state (sb-ext:seed-random-state 11))
         (y (copy-seq (worm.topology:tube-rest-coords tube)))
         (scratch (worm.oracle::make-pressure-scratch 8))
         (eps 1d-6))
    (dotimes (i (length y)) (incf (aref y i) (* 0.006d0 (- (random 1d0 state) 0.5d0))))
    (dotimes (c 3)
      (let* ((v (worm.oracle:compartment-geometry y c 8 scratch t))
             (grad (copy-seq (worm.oracle::pressure-scratch-grad scratch)))
             (hess (worm.oracle::pressure-scratch-hess scratch))
             (base (* 3 c 8)))
        (is (worm.core:approx= v (worm.topology:compartment-volume y c 8) :rel 1d-12)
            "объём ядра давления совпадает с объёмом топологии")
        (dotimes (q 48)
          (let ((yp (copy-seq y)) (ym (copy-seq y)))
            (incf (aref yp (+ base q)) eps) (decf (aref ym (+ base q)) eps)
            (let ((fd (/ (- (worm.topology:compartment-volume yp c 8)
                            (worm.topology:compartment-volume ym c 8))
                         (* 2 eps))))
              (is (< (abs (- fd (aref grad q))) 1d-9) "∂V/∂x[~D]: ~,3E против ~,3E" q (aref grad q) fd))
            ;; столбец гессиана — разность градиентов
            (worm.oracle:compartment-geometry yp c 8 scratch nil)
            (let ((gp (copy-seq (worm.oracle::pressure-scratch-grad scratch))))
              (worm.oracle:compartment-geometry ym c 8 scratch nil)
              (let ((gm (copy-seq (worm.oracle::pressure-scratch-grad scratch)))
                    (worst 0d0))
                (worm.oracle:compartment-geometry y c 8 scratch t)
                (dotimes (r 48)
                  (setf worst (max worst (abs (- (/ (- (aref gp r) (aref gm r)) (* 2 eps))
                                                 (aref hess r q))))))
                (is (< worst 1d-8) "столбец ~D гессиана: ~,2E" q worst)))))))))

(test stiffness-follows-the-measured-profile
  ;; EI постоянна от головы до 2/3 длины и падает к хвосту до половины;
  ;; k_c = 2·EI/(K·R²·Δs), поэтому EI ребра = k_c·K·R²·Δs/2. В ребро идёт
  ;; EI набора с поправкой ei-correction (этап 5): с ней EI всей модели под
  ;; чистым моментом равна заданной.
  (let* ((body (worm.oracle:build-body :params "water-v1"))
         (set (worm.oracle:body-params body))
         (target (* (worm.params:param-value set :bending-ei) (worm.params:param-value set :ei-correction)))
         (tube (worm.oracle:body-tube body))
         (k (worm.oracle:body-long-stiffness body))
         (radius (worm.topology:tube-ring-radius tube))
         (ds (/ 1d0 49)))
    (flet ((ei (i) (/ (* (aref k (* 8 i)) 8 (expt (* 0.5d0 (+ (aref radius i) (aref radius (1+ i)))) 2) ds) 2)))
      (is (worm.core:approx= target (ei 24) :rel 1d-12) "середина тела: EI из набора")
      (is (worm.core:approx= target (ei 2) :rel 1d-12) "голова: та же EI")
      (is (< 0.5d0 (/ (ei 48) target) 0.6d0) "кончик хвоста: почти вдвое мягче")
      (is (> (aref k 0) (aref k (* 8 24))) "к тонким концам ребро жёстче"))))

(test actuation-drives-the-two-sides-in-antiphase
  ;; Дорсальная и вентральная клетки одного номера лежат на одном уровне
  ;; (MDR и MVR по 24 клетки) и тянут в противофазе.
  (let* ((body (worm.oracle:build-body :params "water-v1" :amplitude 10d0 :frequency 1d0
                                       :wavelength 1d0))
         (model (worm.oracle:body-model body))
         (syn (worm.core:make-dvec 95))
         (muscles (worm.anatomy:muscle-binding-muscles (worm.oracle:body-binding body))))
    (funcall (worm.core:compiled-model-neural-output model) (worm.core:make-dvec 0)
             (worm.core:make-dvec (worm.core:compiled-model-n-observables model))
             (worm.core:make-dvec 0) (worm.oracle:body-pvec body)
             (worm.core:dvec-from '(0.1d0 0d0 0)) syn)
    (is (every (lambda (x) (<= 0d0 x 10d0)) syn) "натяжение мышцы неотрицательно и не больше амплитуды")
    (loop for k from 1 to 24
          for d = (position (intern (format nil "MDR~2,'0D" k) :keyword) muscles :key #'worm.anatomy:muscle-name)
          for v = (position (intern (format nil "MVR~2,'0D" k) :keyword) muscles :key #'worm.anatomy:muscle-name)
          do (is (or (zerop (aref syn d)) (zerop (aref syn v))) "клетки ~D тянут одновременно" k)
             (is (plusp (+ (aref syn d) (aref syn v))) "клетки ~D обе молчат" k))))

;;;; tests/solver.lisp — решатель на эталонных задачах.
;;;;
;;;; Условия готовности этапа 2:
;;;;   ленточная LU совпадает с плотной на случайных ленточных матрицах,
;;;;     включая требующие перестановок;
;;;;   на эталонной задаче ошибка убывает с первым порядком по h;
;;;;   1000 шагов не выделяют памяти;
;;;;   NaN превращается в numerical-blowup; halve-timestep и
;;;;     rollback-to-checkpoint работают.

(in-package #:worm.tests)

(def-suite solver :in worm :description "worm.solver.")
(in-suite solver)

;;; Линейная алгебра

(defun random-band (n kl ku state &key zero-diagonal)
  "Случайная лента и та же матрица целиком."
  (let ((ab (worm.core:make-band n kl ku))
        (full (worm.core:make-dmat n n)))
    (dotimes (j n)
      (loop for i from (max 0 (- j ku)) to (min (1- n) (+ j kl))
            do (let ((v (if (and zero-diagonal (= i j)) 0d0 (- (random 2d0 state) 1d0))))
                 (setf (worm.core:band-ref ab i j kl ku) v
                       (aref full i j) v))))
    (values ab full)))

(defun random-vector (n state)
  (let ((v (worm.core:make-dvec n)))
    (dotimes (i n v) (setf (aref v i) (- (random 2d0 state) 1d0)))))

(test band-lu-matches-dense
  (let ((state (sb-ext:seed-random-state 20261016))
        (pivoted 0))
    (loop for (n kl ku zero-diagonal) in '((40 3 2 nil) (40 3 2 t) (60 1 1 t) (30 0 2 nil)
                                           (30 2 0 nil) (25 5 5 t) (1 0 0 nil) (50 7 3 t))
          do (dotimes (trial 5)
               (multiple-value-bind (ab full) (random-band n kl ku state :zero-diagonal zero-diagonal)
                 (when (and zero-diagonal (= n 1)) (setf (aref full 0 0) 1d0))
                 (let* ((b (random-vector n state))
                        (x-band (copy-seq b)) (x-dense (copy-seq b))
                        (ab0 (copy-seq ab))
                        (ipiv (worm.core:make-ivec n)) (ipiv-d (worm.core:make-ivec n)))
                   (is (null (worm.solver:band-factor! ab n kl ku ipiv)))
                   (worm.solver:band-solve! ab n kl ku ipiv x-band)
                   (is (null (worm.solver:dense-factor! full n ipiv-d)))
                   (worm.solver:dense-solve! full n ipiv-d x-dense)
                   (is (< (max-difference x-band x-dense) (* 1d-9 (max 1d0 (reduce #'max x-dense :key #'abs))))
                       "n = ~D, ленты ~D/~D: ленточное решение расходится с плотным" n kl ku)
                   (let ((r (worm.solver:band-multiply ab0 n kl ku x-band (worm.core:make-dvec n))))
                     ;; невязка относительно масштаба |A|·|x|: случайные матрицы
                     ;; с нулевой диагональю бывают плохо обусловлены
                     (let ((scale (* (reduce #'max ab0 :key #'abs) (reduce #'max x-band :key #'abs)
                                     (+ kl ku 1))))
                       (is (< (max-difference r b) (* 1d-13 scale))
                           "невязка ~,2E при масштабе ~,2E" (max-difference r b) scale)))
                   (when (loop for i below n thereis (/= i (aref ipiv i)))
                     (incf pivoted))))))
    (is (> pivoted 10) "перестановки строк действительно происходили: ~D раз" pivoted)))

(test band-lu-reports-singular-matrix
  (let* ((n 6) (kl 1) (ku 1)
         (ab (worm.core:make-band n kl ku))
         (ipiv (worm.core:make-ivec n)))
    (dotimes (i n) (setf (worm.core:band-ref ab i i kl ku) 1d0))
    (setf (worm.core:band-ref ab 3 3 kl ku) 0d0)
    (is (eql 3 (worm.solver:band-factor! ab n kl ku ipiv)) "нулевой столбец 3")
    (let ((a (worm.core:make-dmat 2 2 1d0)))
      (is (eql 1 (worm.solver:dense-factor! a 2 (worm.core:make-ivec 2)))))))

;;; Порядок сходимости

(defun run-chain (n h time &key (c 1d0) (eta 0.1d0) (k 50d0))
  (let* ((y0 (chain-exact n 0d0 c eta k))
         (it (worm.solver:make-integrator (chain-model n) :params (list c eta k)
                                          :y-mech y0 :h h :checkpoint-every 1000)))
    (worm.solver:run-steps it (round time h))
    (is (worm.core:approx= time (worm.solver:integrator-time it) :rel 1d-12))
    (max-difference (worm.solver:integrator-y-mech it) (chain-exact n time c eta k))))

(test chain-converges-with-first-order
  (let* ((hs '(4d-3 2d-3 1d-3 5d-4 2.5d-4))
         (errors (mapcar (lambda (h) (run-chain 20 h 0.2d0)) hs)))
    (is (< (car (last errors)) 1d-3) "ошибка при наименьшем h: ~,3E" (car (last errors)))
    (loop for (e1 e2) on errors while e2
          do (is (< 1.8d0 (/ e1 e2) 2.2d0)
                 "отношение ошибок ~,3F при уменьшении h вдвое; ошибки ~{~,2E~^ ~}" (/ e1 e2) errors))))

(defun run-stiff (h time &key (l1 1d6))
  (let ((it (worm.solver:make-integrator (stiff-model) :params (list l1) :y-mech '(1d0 1d0) :h h
                                                       :checkpoint-every 1000)))
    (worm.solver:run-steps it (round time h))
    (worm.solver:integrator-y-mech it)))

(test stiff-system-is-stable-and-first-order
  ;; h·λ₁ = 10⁴: явный метод разошёлся бы сразу, неявный обязан идти по медленной моде.
  (let* ((hs '(1d-2 5d-3 2.5d-3))
         (errors (mapcar (lambda (h) (max-difference (run-stiff h 1d0) (stiff-exact 1d0 1d6))) hs)))
    (is (< (first errors) 5d-3) "ошибка при h = 10⁻²: ~,3E" (first errors))
    (loop for (e1 e2) on errors while e2
          do (is (< 1.8d0 (/ e1 e2) 2.2d0) "отношение ~,3F" (/ e1 e2)))
    (let ((y (run-stiff 1d-2 1d0)))
      (is (< (abs (- (aref y 0) (aref y 1))) 1d-4) "быстрая мода подчинена медленной"))))

(defun run-coupled (h time &key (neural-every 1) (a 2d0) (b 3d0) (threshold 0.4d0)
                                (checkpoint-every (* 10 neural-every)))
  (let ((it (worm.solver:make-integrator (coupled-model) :params (list a b threshold)
                                         :y-mech '(1d0) :y-neural '(1d0) :h h
                                         :neural-every neural-every
                                         :checkpoint-every checkpoint-every)))
    (worm.solver:run-steps it (round time h))
    it))

(test splitting-is-first-order-even-with-stale-neural-matrix
  (dolist (r '(1 10))
    (let* ((hs '(4d-3 2d-3 1d-3 5d-4))
           (runs (mapcar (lambda (h) (run-coupled h 0.5d0 :neural-every r)) hs))
           (x-errors (mapcar (lambda (it) (abs (- (aref (worm.solver:integrator-y-neural it) 0)
                                                  (coupled-x-exact 0.5d0 2d0 1d0))))
                             runs))
           (ys (mapcar (lambda (it) (aref (worm.solver:integrator-y-mech it) 0)) runs)))
      (loop for (e1 e2) on x-errors while e2
            do (is (< 1.7d0 (/ e1 e2) 2.3d0) "R = ~D: нервы, отношение ошибок ~,3F" r (/ e1 e2)))
      ;; механика: точного решения нет, проверяется самосходимость по разностям
      (let ((d1 (abs (- (first ys) (second ys))))
            (d2 (abs (- (second ys) (third ys))))
            (d3 (abs (- (third ys) (fourth ys)))))
        (is (< 1.7d0 (/ d1 d2) 2.3d0) "R = ~D: механика, ~,3F" r (/ d1 d2))
        (is (< 1.7d0 (/ d2 d3) 2.3d0) "R = ~D: механика, ~,3F" r (/ d2 d3))))))

(test step-order-observables-then-discrete
  ;; z на шаге n считается по наблюдаемой y(tₙ): после шага, на котором y
  ;; впервые опустилась ниже порога, z ещё 1, а шагом позже — 0.
  (let ((it (worm.solver:make-integrator (coupled-model) :params '(2d0 3d0 0.9d0)
                                         :y-mech '(1d0) :y-neural '(1d0) :h 1d-3))
        (crossed nil))
    (loop repeat 200
          do (let ((y-before (aref (worm.solver:integrator-y-mech it) 0)))
               (worm.solver:step! it)
               (is (= (if (> y-before 0.9d0) 1d0 0d0) (aref (worm.solver:integrator-z it) 0)))
               (when (<= y-before 0.9d0) (setf crossed t))))
    (is-true crossed)))

;;; Память

(defun bytes-consed-by (thunk)
  (let ((before (sb-ext:get-bytes-consed)))
    (funcall thunk)
    (- (sb-ext:get-bytes-consed) before)))

(test steps-allocate-nothing
  (dolist (make (list (lambda () (worm.solver:make-integrator
                                  (chain-model 50) :params '(1d0 0.1d0 50d0)
                                  :y-mech (chain-exact 50 0d0 1d0 0.1d0 50d0) :h 1d-4))
                      (lambda () (worm.solver:make-integrator
                                  (coupled-model) :params '(2d0 3d0 0.4d0) :y-mech '(1d0)
                                  :y-neural '(1d0) :h 1d-4))))
    (let ((it (funcall make)))
      (loop repeat 200 do (worm.solver:step! it))   ; прогрев: первая контрольная точка и т. п.
      ;; SBCL учитывает выделение блоками по 32 КБ, поэтому замер на 1000
      ;; шагах шумит. Проверяется, что 50 000 шагов выделяют не больше
      ;; одного блока: даже одно упакованное число на шаг дало бы 800 КБ.
      (let ((bytes (bytes-consed-by (lambda () (loop repeat 50000 do (worm.solver:step! it))))))
        (is (<= bytes 32768) "~A: 50 000 шагов выделили ~D байт"
            (worm.core:compiled-model-name (worm.solver:integrator-model it)) bytes))
      ;; RUN-STEPS тратит постоянную память на вызов, но не на шаг
      (let ((bytes (bytes-consed-by (lambda () (worm.solver:run-steps it 50000)))))
        (is (<= bytes 32768) "run-steps: ~D байт на 50 000 шагов" bytes)))))

;;; Сбои и перезапуски

(defun decay (&rest model-args &key (h 1d-3) (checkpoint-every 50) &allow-other-keys)
  (setf model-args (copy-list model-args))
  (remf model-args :h) (remf model-args :checkpoint-every)
  (worm.solver:make-integrator (apply #'decay-model model-args) :y-mech '(1d0) :h h
                               :checkpoint-every checkpoint-every :neural-every 1))

(test nan-becomes-numerical-blowup
  (let ((it (decay :nan-at-call 5)))
    (handler-case (progn (worm.solver:run-steps it 100) (fail "сбой не замечен"))
      (worm.solver:numerical-blowup (c)
        (is (member (worm.solver:numerical-blowup-cause c) '(:nan :invalid)))
        (is (= 4 (worm.solver:numerical-blowup-step c)) "шаг сбоя")
        (is (search "численный сбой" (princ-to-string c))))))
  (let ((it (decay :trap-above-h 1d-4 :h 1d-3)))
    (handler-case (progn (worm.solver:run-steps it 10) (fail "ловушка не сработала"))
      (worm.solver:numerical-blowup (c)
        (is (eq :invalid (worm.solver:numerical-blowup-cause c)))
        (is (eq :mech-matrix (worm.solver:numerical-blowup-where c)) "подсистема сбоя")))))

(test halve-timestep-recovers
  (let ((it (decay :trap-above-h 1.1d-3 :h 4d-3)))
    (is (eq :done (worm.solver:run-steps it 100 :policy (worm.solver:make-batch-policy))))
    (is (= 1d-3 (worm.solver:integrator-h it)))
    (is (equal '((0 . 4d-3) (0 . 2d-3) (0 . 1d-3)) (worm.solver:integrator-dt-history it)))
    (is (equal '((0 worm.solver:halve-timestep :invalid) (0 worm.solver:halve-timestep :invalid))
               (worm.solver:integrator-restarts it)))
    (is (= 100 (worm.solver:integrator-step it)))
    (is (worm.core:approx= 0.1d0 (worm.solver:integrator-time it) :rel 1d-12))
    (is (< (abs (- (aref (worm.solver:integrator-y-mech it) 0) (exp -0.1d0))) 1d-3))))

(defun decay-failing-check-once (at-step h)
  "decay, у которой проверка состояния один раз отказывает на шаге AT-STEP —
уже после того, как шаг изменил состояние."
  (let ((m (decay-model)) (failed nil) (calls 0))
    (worm.solver:make-integrator
     (worm.core:make-compiled-model
      :name 'decay-check :n-mech 1
      :mech-forces (worm.core:compiled-model-mech-forces m)
      :mech-matrix (worm.core:compiled-model-mech-matrix m)
      :check-state (lambda (y p) (declare (ignore y p))
                     (incf calls)
                     (if (and (not failed) (= calls (1+ at-step)))
                         (progn (setf failed t) (values :volume-collapse 0))
                         nil)))
     :y-mech '(1d0) :h h :neural-every 1 :checkpoint-every 50)))

(test halve-timestep-restarts-from-the-step-start
  ;; Сбой на шаге 4 после обновления состояния: повтор с h/2 обязан начаться
  ;; с состояния начала шага, иначе шаг 4 был бы сделан дважды. Сверка —
  ;; побитово с прогоном, где h уменьшен вручную перед шагом 4.
  (let ((it (decay-failing-check-once 4 2d-3))
        (clean (decay :h 2d-3)))
    (is (eq :done (worm.solver:run-steps it 50 :policy (worm.solver:make-batch-policy))))
    (is (= 1 (length (worm.solver:integrator-restarts it))))
    (worm.solver:run-steps clean 4)
    (setf (worm.solver:integrator-h clean) 1d-3)
    (worm.solver:run-steps clean 46)
    (is (= (worm.solver:integrator-time clean) (worm.solver:integrator-time it)))
    (is (= (aref (worm.solver:integrator-y-mech clean) 0) (aref (worm.solver:integrator-y-mech it) 0))
        "повтор шага начался не с его начала")))

(test batch-policy-gives-up-after-three-halvings
  (let ((it (decay :trap-above-h 1d-4 :h 4d-3)))
    (is (eq :aborted (worm.solver:run-steps it 100 :policy (worm.solver:make-batch-policy))))
    (is (eq :aborted (worm.solver:integrator-status it)))
    (is (= 4 (length (worm.solver:integrator-restarts it))))
    (is (eq 'worm.solver:abort-run (second (car (last (worm.solver:integrator-restarts it))))))))

(test rollback-to-checkpoint-reproduces-the-clean-run
  (let ((clean (decay))
        (faulty (decay :nan-at-call 130)))
    (worm.solver:run-steps clean 300)
    (is (eq :done (worm.solver:run-steps faulty 300
                                         :policy (constantly 'worm.solver:rollback-to-checkpoint))))
    (is (equal '((129 worm.solver:rollback-to-checkpoint :nan))
               (mapcar (lambda (r) (if (eq (third r) :invalid) (list (first r) (second r) :nan) r))
                       (worm.solver:integrator-restarts faulty))))
    (is (= 300 (worm.solver:integrator-step faulty)))
    (is (= (aref (worm.solver:integrator-y-mech clean) 0)
           (aref (worm.solver:integrator-y-mech faulty) 0))
        "после отката прогон побитово совпадает с прогоном без сбоя")))

(test human-choice-works-like-a-policy
  ;; В REPL перезапуск выбирает человек; здесь его заменяет обработчик снаружи.
  (let ((it (decay :trap-above-h 1.1d-3 :h 2d-3)))
    (handler-bind ((worm.solver:numerical-blowup
                     (lambda (c) (invoke-restart (find-restart 'worm.solver:halve-timestep c)))))
      (worm.solver:run-steps it 20))
    (is (= 1d-3 (worm.solver:integrator-h it)))))

(test clamp-and-abort-restarts
  (let ((it (decay :nan-at-call 10)))
    (worm.solver:run-steps it 20 :policy (constantly 'worm.solver:clamp-and-continue))
    (is (= 20 (worm.solver:integrator-step it)))
    (is-true (worm.solver:integrator-unreliable-p it))
    (is (every (lambda (x) (= x x)) (worm.solver:integrator-y-mech it))))
  (let ((it (decay :nan-at-call 10)))
    (is (eq :aborted (worm.solver:run-steps it 20 :policy (constantly 'worm.solver:abort-run))))
    (is (= 9 (worm.solver:integrator-step it)))))

(test singular-matrix-is-a-blowup
  (let ((it (worm.solver:make-integrator
             (worm.core:make-compiled-model
              :name 'singular :n-mech 2 :lower-band 1 :upper-band 1
              :mech-forces (lambda (y syn p g) (declare (ignore y syn p g)))
              :mech-matrix (lambda (y syn p clock ab) (declare (ignore y syn p clock ab))))
             :h 1d-3)))
    (handler-case (progn (worm.solver:run-steps it 1) (fail "вырожденная матрица прошла"))
      (worm.solver:numerical-blowup (c)
        (is (eq :singular-matrix (worm.solver:numerical-blowup-cause c)))))))

(test check-state-reports-model-specific-failures
  (let ((it (worm.solver:make-integrator
             (let ((m (decay-model)))
               (worm.core:make-compiled-model
                :name 'collapse :n-mech 1
                :mech-forces (worm.core:compiled-model-mech-forces m)
                :mech-matrix (worm.core:compiled-model-mech-matrix m)
                :check-state (lambda (y p) (declare (ignore p))
                               (if (< (aref y 0) 0.5d0) (values :volume-collapse 0) nil))))
             :y-mech '(1d0) :h 1d-2)))
    (handler-case (progn (worm.solver:run-steps it 1000) (fail "проверка модели не сработала"))
      (worm.solver:numerical-blowup (c)
        (is (eq :volume-collapse (worm.solver:numerical-blowup-cause c)))
        (is (< 60 (worm.solver:numerical-blowup-step c) 80))))))

;;; Контрольные точки

(test checkpoint-ring-keeps-the-latest
  (let ((it (worm.solver:make-integrator (chain-model 5) :params '(1d0 0.1d0 50d0)
                                         :y-mech (chain-exact 5 0d0 1d0 0.1d0 50d0) :h 1d-3
                                         :neural-every 5 :checkpoint-every 10
                                         :checkpoint-capacity 3)))
    (is (equal '(0) (worm.solver:checkpoint-steps it)) "точка шага 0 есть сразу")
    (worm.solver:run-steps it 55)
    (is (equal '(50 40 30) (worm.solver:checkpoint-steps it)))
    (let ((y40 nil))
      (worm.solver:restore-checkpoint it :back 1)
      (is (= 40 (worm.solver:integrator-step it)))
      (setf y40 (copy-seq (worm.solver:integrator-y-mech it)))
      (setf (aref (worm.solver:integrator-params it) 2) 7d0)
      (worm.solver:restore-checkpoint it :back 1)
      (is (= 7d0 (aref (worm.solver:integrator-params it) 2)) "откат по умолчанию не трогает параметры")
      (worm.solver:restore-checkpoint it :back 1 :params t)
      (is (= 50d0 (aref (worm.solver:integrator-params it) 2)))
      (is (equalp y40 (worm.solver:integrator-y-mech it))))))

(test rollback-and-continue-matches-uninterrupted-run
  ;; Пересборка нервной матрицы привязана к абсолютному номеру шага, а Q
  ;; кратно R, поэтому продолжение после отката идёт побитово так же.
  (let ((a (run-coupled 1d-3 0.04d0 :neural-every 5 :checkpoint-every 10))
        (b (run-coupled 1d-3 0.04d0 :neural-every 5 :checkpoint-every 10)))
    (worm.solver:restore-checkpoint b :back 1)
    (is (= 30 (worm.solver:integrator-step b)))
    (worm.solver:run-steps b 10)
    (is (= 40 (worm.solver:integrator-step b)))
    (is (= (aref (worm.solver:integrator-y-mech a) 0) (aref (worm.solver:integrator-y-mech b) 0)))
    (is (= (aref (worm.solver:integrator-y-neural a) 0) (aref (worm.solver:integrator-y-neural b) 0)))))

(test integrator-checks-its-arguments
  (signals error (worm.solver:make-integrator (chain-model 3) :neural-every 3 :checkpoint-every 10))
  (signals error (worm.solver:make-integrator (chain-model 3) :y-mech '(1d0 2d0)))
  (signals error (worm.solver:make-integrator (worm.core:make-compiled-model :name 'empty))))

;;; Скорости прошлого шага (aux, решение 22)

(defun lagged-model ()
  "ẏ = −y + c·obs, где obs — скорость y с прошлого шага, прочитанная через aux.
При c = 0 это затухание; при c ≠ 0 правая часть зависит от прошлой скорости,
и порядок шагов после отката должен её восстановить."
  (worm.core:make-compiled-model
   :name 'lagged :n-mech 1 :n-aux 1 :n-observables 1 :n-syn 1
   :observables
   (lambda (y aux p obs)
     (declare (type worm.core:dvec y aux p obs) (ignore y p))
     (incf (aref obs 0) (aref aux 0))
     (values))
   :neural-output
   (lambda (yn obs z p clock syn)
     (declare (type worm.core:dvec yn obs z p clock syn) (ignore yn z p clock))
     (incf (aref syn 0) (aref obs 0))
     (values))
   :mech-forces
   (lambda (y syn p g)
     (declare (type worm.core:dvec y syn p g))
     (incf (aref g 0) (+ (- (aref y 0)) (* (aref p 0) (aref syn 0))))
     (values))
   :mech-matrix
   (lambda (y syn p clock ab)
     (declare (type worm.core:dvec y syn p clock ab) (ignore y syn p))
     (incf (aref ab (worm.core:band-index 0 0 0 0)) (+ 1d0 (aref clock worm.core:+clock-h+)))
     (values))))

(test aux-holds-last-step-velocities
  (let ((it (worm.solver:make-integrator (lagged-model) :params '(0.3d0) :y-mech '(1d0) :h 1d-2
                                                        :checkpoint-every 5 :neural-every 5)))
    (is (= 0d0 (aref (worm.solver:integrator-aux it) 0)) "до первого шага скоростей нет")
    (let ((y0 (aref (worm.solver:integrator-y-mech it) 0)))
      (worm.solver:step! it)
      (is (< (abs (- (/ (- (aref (worm.solver:integrator-y-mech it) 0) y0) 1d-2)
                     (aref (worm.solver:integrator-aux it) 0)))
             1d-12)
          "aux = Δy/h"))
    (let ((v (aref (worm.solver:integrator-aux it) 0)))
      (worm.solver:step! it)
      (is (= v (aref (worm.solver:integrator-syn it) 0)) "модель видит скорость прошлого шага"))
    ;; откат восстанавливает и aux: продолжение побитово как без остановки
    (worm.solver:run-steps it 18)
    (let ((y20 (aref (worm.solver:integrator-y-mech it) 0))
          (a20 (aref (worm.solver:integrator-aux it) 0)))
      (worm.solver:run-steps it 7)
      (worm.solver:restore-checkpoint it)
      (is (= 25 (worm.solver:integrator-step it)))
      (worm.solver:restore-checkpoint it :back 1)
      (is (= 20 (worm.solver:integrator-step it)))
      (is (= y20 (aref (worm.solver:integrator-y-mech it) 0)))
      (is (= a20 (aref (worm.solver:integrator-aux it) 0)) "aux в контрольной точке"))))

(test integrator-starts-from-a-given-step
  (let* ((a (worm.solver:make-integrator (lagged-model) :params '(0.3d0) :y-mech '(1d0) :h 1d-2
                                                       :checkpoint-every 5 :neural-every 5)))
    (worm.solver:run-steps a 12)
    (let ((b (worm.solver:make-integrator (lagged-model) :params '(0.3d0) :h 1d-2 :checkpoint-every 5 :neural-every 5
                                          :y-mech (worm.solver:integrator-y-mech a)
                                          :aux (worm.solver:integrator-aux a)
                                          :time (worm.solver:integrator-time a) :step 12)))
      (is (equal '(12) (worm.solver:checkpoint-steps b)))
      (worm.solver:run-steps a 8)
      (worm.solver:run-steps b 8)
      (is (= 20 (worm.solver:integrator-step b)))
      (is (equal '(20 15 12) (worm.solver:checkpoint-steps b)))
      (is (= (aref (worm.solver:integrator-y-mech a) 0) (aref (worm.solver:integrator-y-mech b) 0))
          "продолжение с заданного шага побитово как без остановки"))))

(test integrator-rejects-partial-aux
  (signals error (worm.solver:make-integrator
                  (worm.core:make-compiled-model :name 'odd :n-mech 2 :n-aux 1
                                                 :mech-forces (lambda (&rest r) (declare (ignore r)) (values))
                                                 :mech-matrix (lambda (&rest r) (declare (ignore r)) (values))))))

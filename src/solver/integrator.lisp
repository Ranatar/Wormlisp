;;;; src/solver/integrator.lisp — линейно-неявный шаг с расщеплением Ли.
;;;;
;;;; Архитектура, «Решатель». Шаг h — две подсистемы по очереди:
;;;;
;;;;   1. наблюдаемые по y-mech(tₙ) и скоростям прошлого шага aux;
;;;;   2. нервы до tₙ₊₁: дискретное состояние z (уровень 2), затем
;;;;      линейно-неявный шаг y-neural (уровень 3) с плотной LU, матрица
;;;;      которой пересобирается раз в R шагов (по абсолютному номеру шага),
;;;;      затем выход syn при tₙ₊₁ — постоянный в пределах шага;
;;;;   3. механика: (B − h·∂G/∂y)·Δy = h·G, ленточная LU, y-mech += Δy,
;;;;      aux = Δy/h, если модель их читает (n-aux = n-mech);
;;;;   4. проверки: ловушки плавающей точки, нечисловые значения, check-state;
;;;;   5. время и номер шага, контрольная точка раз в Q шагов.
;;;;
;;;; Перед шагом его начальное состояние копируется в запасные буферы:
;;;; перезапуск halve-timestep повторяет шаг с h/2 ровно с того же места.
;;;; Кольцо из C контрольных точек — в заранее выделенных массивах; откат
;;;; восстанавливает состояние, но не параметры (если не попросить :params t).
;;;;
;;;; Шаг не выделяет память: все буферы выделяются в MAKE-INTEGRATOR,
;;;; перезапуски устанавливаются один раз на вызов RUN-STEPS, а не на шаг.

(in-package #:worm.solver)

(defstruct (checkpoint (:constructor %make-checkpoint) (:copier nil))
  (valid nil)
  (time 0d0 :type double-float)
  (step 0 :type fixnum)
  (h 0d0 :type double-float)
  (y-mech nil :type dvec)
  (y-neural nil :type dvec)
  (z nil :type dvec)
  (aux nil :type dvec)
  (params nil :type dvec))

(defstruct (integrator (:constructor %make-integrator) (:copier nil))
  (model nil :type compiled-model :read-only t)
  (params nil :type dvec)
  ;; состояние
  (y-mech nil :type dvec :read-only t)
  (y-neural nil :type dvec :read-only t)
  (z nil :type dvec :read-only t)
  (aux nil :type dvec :read-only t)       ; скорости y-mech прошлого шага (lagged)
  (time 0d0 :type double-float)
  (step 0 :type fixnum)
  (h 1d-4 :type double-float)
  (clock (make-dvec 3) :type dvec :read-only t)  ; tₙ, h, номер шага — для функций модели
  ;; связь подсистем
  (obs nil :type dvec :read-only t)
  (syn nil :type dvec :read-only t)
  ;; механика
  (n-mech 0 :type fixnum :read-only t)
  (kl 0 :type fixnum :read-only t)
  (ku 0 :type fixnum :read-only t)
  (g nil :type dvec :read-only t)
  (ab nil :type dvec :read-only t)
  (ipiv-mech nil :type ivec :read-only t)
  ;; нервы уровня 3
  (n-neural 0 :type fixnum :read-only t)
  (f-neural nil :type dvec :read-only t)
  (jac nil :type dmat :read-only t)
  (ipiv-neural nil :type ivec :read-only t)
  (neural-every 10 :type fixnum :read-only t)   ; R
  (factor-valid nil)
  (factor-h 0d0 :type double-float)
  ;; начало текущего шага
  (y-mech0 nil :type dvec :read-only t)
  (y-neural0 nil :type dvec :read-only t)
  (z0 nil :type dvec :read-only t)
  (aux0 nil :type dvec :read-only t)
  (time0 0d0 :type double-float)
  ;; контрольные точки
  (checkpoint-every 100 :type fixnum :read-only t)   ; Q
  (ring #() :type simple-vector :read-only t)        ; C точек
  (ring-next 0 :type fixnum)
  ;; журнал прогона
  (phase :idle :type keyword)
  (%dt-history nil)          ; ((с-шага . h) …), новые первыми
  (%restarts nil)            ; ((шаг перезапуск причина) …), новые первыми
  (unreliable-p nil)
  (status :ready :type keyword))

(defmethod print-object ((it integrator) stream)
  (print-unreadable-object (it stream :type t)
    (format stream "~A шаг ~D, t = ~,6F с, h = ~,3E с, ~(~A~)"
            (compiled-model-name (integrator-model it)) (integrator-step it)
            (integrator-time it) (integrator-h it) (integrator-status it))))

(defun integrator-dt-history (it)
  "Шаги по времени за прогон: список (с-какого-шага . h) по порядку."
  (reverse (integrator-%dt-history it)))

(defun integrator-restarts (it)
  "Применённые перезапуски: список (шаг перезапуск причина) по порядку."
  (reverse (integrator-%restarts it)))

(defun copy-or-zero (source n what)
  (cond ((null source) (make-dvec n))
        ((= (length source) n) (dvec-from source))
        (t (error "~A: длина ~D, а модель ждёт ~D" what (length source) n))))

(defun make-checkpoint-slot (it)
  (%make-checkpoint :y-mech (make-dvec (length (integrator-y-mech it)))
                    :y-neural (make-dvec (length (integrator-y-neural it)))
                    :z (make-dvec (length (integrator-z it)))
                    :aux (make-dvec (length (integrator-aux it)))
                    :params (make-dvec (length (integrator-params it)))))

(defun make-integrator (model &key params y-mech y-neural z aux (h 1d-4) (time 0d0) (step 0)
                                   (neural-every 10) (checkpoint-every 100)
                                   (checkpoint-capacity 10))
  "Интегратор для MODEL с начальным состоянием и параметрами.
NEURAL-EVERY — R, CHECKPOINT-EVERY — Q (кратно R), CHECKPOINT-CAPACITY — C.
STEP — номер шага, с которого продолжается прогон (например, из контрольной
точки на диске): от него зависят расписания пересборки и контрольных точек.
Сразу сохраняет контрольную точку начального шага, так что откат есть всегда."
  (unless (and (plusp neural-every) (plusp checkpoint-every)
               (zerop (mod checkpoint-every neural-every)))
    (error "интервал контрольных точек Q = ~D должен быть кратен R = ~D"
           checkpoint-every neural-every))
  (unless (plusp checkpoint-capacity) (error "нужна хотя бы одна контрольная точка"))
  (unless (member (compiled-model-n-aux model) (list 0 (compiled-model-n-mech model)))
    (error "у модели ~A n-aux = ~D: скорости прошлого шага либо не нужны (0), либо все (n-mech = ~D)"
           (compiled-model-name model) (compiled-model-n-aux model) (compiled-model-n-mech model)))
  (unless (and (compiled-model-mech-forces model) (compiled-model-mech-matrix model))
    (error "у модели ~A нет механики" (compiled-model-name model)))
  (let* ((nm (compiled-model-n-mech model))
         (nn (compiled-model-n-neural model))
         (kl (compiled-model-lower-band model))
         (ku (compiled-model-upper-band model))
         (it (%make-integrator
              :model model
              :params (dvec-from (or params #()))
              :y-mech (copy-or-zero y-mech nm "y-mech")
              :y-neural (copy-or-zero y-neural nn "y-neural")
              :z (copy-or-zero z (compiled-model-n-discrete model) "z")
              :aux (copy-or-zero aux (compiled-model-n-aux model) "aux")
              :time (float time 0d0) :step step :h (float h 0d0)
              :obs (make-dvec (compiled-model-n-observables model))
              :syn (make-dvec (compiled-model-n-syn model))
              :n-mech nm :kl kl :ku ku
              :g (make-dvec nm) :ab (make-band nm kl ku) :ipiv-mech (make-ivec nm)
              :n-neural nn :f-neural (make-dvec nn) :jac (make-dmat nn nn)
              :ipiv-neural (make-ivec nn) :neural-every neural-every
              :y-mech0 (make-dvec nm) :y-neural0 (make-dvec nn)
              :z0 (make-dvec (compiled-model-n-discrete model))
              :aux0 (make-dvec (compiled-model-n-aux model))
              :checkpoint-every checkpoint-every
              :ring (make-array checkpoint-capacity :initial-element nil))))
    (when (and (plusp nn) (not (and (compiled-model-neural-rhs model)
                                    (compiled-model-neural-jacobian model))))
      (error "у модели ~A есть y-neural, но нет neural-rhs или neural-jacobian"
             (compiled-model-name model)))
    (dotimes (k checkpoint-capacity)
      (setf (aref (integrator-ring it) k) (make-checkpoint-slot it)))
    (setf (integrator-%dt-history it) (list (cons 0 (integrator-h it))))
    (save-checkpoint it)
    it))

;;; Контрольные точки

(defun save-checkpoint (it)
  "Сохранить полное состояние в следующую ячейку кольца."
  (declare (type integrator it) (optimize speed))
  (let* ((ring (integrator-ring it))
         (slot (aref ring (integrator-ring-next it))))
    (declare (type checkpoint slot))
    (replace (checkpoint-y-mech slot) (integrator-y-mech it))
    (replace (checkpoint-y-neural slot) (integrator-y-neural it))
    (replace (checkpoint-z slot) (integrator-z it))
    (replace (checkpoint-aux slot) (integrator-aux it))
    (replace (checkpoint-params slot) (integrator-params it))
    (setf (checkpoint-time slot) (integrator-time it)
          (checkpoint-step slot) (integrator-step it)
          (checkpoint-h slot) (integrator-h it)
          (checkpoint-valid slot) t)
    (setf (integrator-ring-next it) (mod (1+ (integrator-ring-next it)) (length ring)))
    it))

(defun checkpoints-newest-first (it)
  (let* ((ring (integrator-ring it)) (c (length ring)))
    (loop for k from 1 to c
          for slot = (aref ring (mod (- (integrator-ring-next it) k) c))
          when (checkpoint-valid slot) collect slot)))

(defun checkpoint-count (it) (length (checkpoints-newest-first it)))

(defun checkpoint-steps (it)
  "Номера шагов сохранённых точек, новые первыми."
  (mapcar #'checkpoint-step (checkpoints-newest-first it)))

(defun latest-checkpoint-step (it) (first (checkpoint-steps it)))

(defun restore-checkpoint (it &key (back 0) params keep-h)
  "Восстановить состояние из точки номер BACK (0 — последняя). Параметры
восстанавливаются только при PARAMS = T. Шаг h берётся из точки, если не
задано KEEP-H."
  (let ((slot (nth back (checkpoints-newest-first it))))
    (unless slot (error "контрольной точки номер ~D нет" back))
    (replace (integrator-y-mech it) (checkpoint-y-mech slot))
    (replace (integrator-y-neural it) (checkpoint-y-neural slot))
    (replace (integrator-z it) (checkpoint-z slot))
    (replace (integrator-aux it) (checkpoint-aux slot))
    (when params (replace (integrator-params it) (checkpoint-params slot)))
    (setf (integrator-time it) (checkpoint-time slot)
          (integrator-step it) (checkpoint-step slot)
          (integrator-factor-valid it) nil)
    (unless (or keep-h (= (integrator-h it) (checkpoint-h slot)))
      (setf (integrator-h it) (checkpoint-h slot))
      (push (cons (integrator-step it) (integrator-h it)) (integrator-%dt-history it)))
    it))

;;; Шаг

(declaim (inline non-finite-p))
(defun non-finite-p (x)
  "NaN или бесконечность: показатель из одних единиц. Сравнения с NaN не
нужны, поэтому проверка не задевает ловушку недопустимой операции."
  (declare (type double-float x))
  (= (ldb (byte 11 20) (sb-kernel:double-float-high-bits x)) #x7ff))

(defun first-non-finite (v)
  (declare (type dvec v) (optimize speed))
  (dotimes (i (length v) -1)
    (when (non-finite-p (aref v i)) (return i))))

(defun blowup (it cause &optional where)
  (error 'numerical-blowup :cause cause :step (integrator-step it)
                           :time (integrator-time it) :h (integrator-h it)
                           :where where))

(defun remember-step-start (it)
  (declare (type integrator it) (optimize speed))
  (replace (integrator-y-mech0 it) (integrator-y-mech it))
  (replace (integrator-y-neural0 it) (integrator-y-neural it))
  (replace (integrator-z0 it) (integrator-z it))
  (replace (integrator-aux0 it) (integrator-aux it))
  (setf (integrator-time0 it) (integrator-time it))
  nil)  ; не возвращать число: оно упаковалось бы в кучу

(defun return-to-step-start (it)
  (replace (integrator-y-mech it) (integrator-y-mech0 it))
  (replace (integrator-y-neural it) (integrator-y-neural0 it))
  (replace (integrator-z it) (integrator-z0 it))
  (replace (integrator-aux it) (integrator-aux0 it))
  (setf (integrator-time it) (integrator-time0 it)))

(defun neural-substep (it)
  "Нервная подсистема до tₙ₊₁ и выход I^syn."
  (declare (type integrator it) (optimize speed))
  (let* ((model (integrator-model it))
         (p (integrator-params it))
         (obs (integrator-obs it))
         (z (integrator-z it))
         (yn (integrator-y-neural it))
         (h (integrator-h it))
         (clock (integrator-clock it))
         (discrete (compiled-model-neural-discrete model))
         (rhs (compiled-model-neural-rhs model))
         (output (compiled-model-neural-output model)))
    (when discrete
      (setf (integrator-phase it) :neural-discrete)
      (funcall (the function discrete) obs z p clock))
    (when rhs
      (let ((n (integrator-n-neural it))
            (jac (integrator-jac it))
            (f (integrator-f-neural it))
            (ipiv (integrator-ipiv-neural it)))
        (declare (type fixnum n) (type dmat jac) (type dvec f))
        (when (or (not (integrator-factor-valid it))
                  (zerop (mod (integrator-step it) (integrator-neural-every it)))
                  (/= h (integrator-factor-h it)))
          (setf (integrator-phase it) :neural-jacobian)
          (fill (sb-ext:array-storage-vector jac) 0d0)
          (funcall (the function (compiled-model-neural-jacobian model)) yn obs z p clock jac)
          ;; I − h·J
          (dotimes (i n)
            (dotimes (j n)
              (setf (aref jac i j) (- (if (= i j) 1d0 0d0) (* h (aref jac i j))))))
          (when (dense-factor! jac n ipiv)
            (setf (integrator-factor-valid it) nil)
            (blowup it :singular-matrix "нервы уровня 3"))
          (setf (integrator-factor-valid it) t
                (integrator-factor-h it) h))
        (setf (integrator-phase it) :neural-rhs)
        (fill f 0d0)
        (funcall (the function rhs) yn obs z p clock f)
        (dotimes (i n) (setf (aref f i) (* h (aref f i))))
        (dense-solve! jac n ipiv f)
        (dotimes (i n) (incf (aref yn i) (aref f i)))))
    (when output
      (setf (integrator-phase it) :neural-output)
      (fill (integrator-syn it) 0d0)
      (funcall (the function output) yn obs z p clock (integrator-syn it)))))

(defun mech-substep (it)
  "Механика: (B − h·∂G/∂y)·Δy = h·G."
  (declare (type integrator it) (optimize speed))
  (let* ((model (integrator-model it))
         (p (integrator-params it))
         (y (integrator-y-mech it))
         (syn (integrator-syn it))
         (g (integrator-g it))
         (ab (integrator-ab it))
         (n (integrator-n-mech it))
         (kl (integrator-kl it))
         (ku (integrator-ku it))
         (h (integrator-h it)))
    (declare (type fixnum n kl ku))
    (setf (integrator-phase it) :mech-matrix)
    (fill ab 0d0)
    (funcall (the function (compiled-model-mech-matrix model)) y syn p (integrator-clock it) ab)
    (setf (integrator-phase it) :mech-forces)
    (fill g 0d0)
    (funcall (the function (compiled-model-mech-forces model)) y syn p g)
    (setf (integrator-phase it) :mech-solve)
    (dotimes (i n) (setf (aref g i) (* h (aref g i))))
    (when (band-factor! ab n kl ku (integrator-ipiv-mech it))
      (blowup it :singular-matrix "механика"))
    (band-solve! ab n kl ku (integrator-ipiv-mech it) g)
    (dotimes (i n) (incf (aref y i) (aref g i)))
    (let ((aux (integrator-aux it)))
      (when (= (length aux) n)
        (let ((inv (/ 1d0 h)))
          (dotimes (i n) (setf (aref aux i) (* inv (aref g i)))))))))

(defun check-step (it)
  (declare (type integrator it) (optimize speed))
  (setf (integrator-phase it) :checks)
  (let ((bad (first-non-finite (integrator-y-mech it))))
    (declare (type fixnum bad))
    (when (>= bad 0) (blowup it :nan (format nil "y-mech[~D]" bad))))
  (let ((bad (first-non-finite (integrator-y-neural it))))
    (declare (type fixnum bad))
    (when (>= bad 0) (blowup it :nan (format nil "y-neural[~D]" bad))))
  (let ((check (compiled-model-check-state (integrator-model it))))
    (when check
      (multiple-value-bind (cause where)
          (funcall (the function check) (integrator-y-mech it) (integrator-params it))
        (when cause (blowup it cause where))))))

(defun finish-step (it)
  (declare (type integrator it) (optimize speed))
  (setf (integrator-time it) (+ (integrator-time0 it) (integrator-h it)))
  (incf (integrator-step it))
  (setf (integrator-phase it) :idle)
  (when (zerop (mod (integrator-step it) (integrator-checkpoint-every it)))
    (save-checkpoint it)))

(defun step! (it)
  "Один шаг интегратора. При сбое сигнализирует numerical-blowup; состояние
начала шага остаётся в запасных буферах для перезапусков RUN-STEPS."
  (declare (type integrator it) (optimize speed))
  (remember-step-start it)
  (let ((clock (integrator-clock it)))
    (setf (aref clock +clock-time+) (integrator-time it)
          (aref clock +clock-h+) (integrator-h it)
          (aref clock +clock-step+) (float (integrator-step it) 0d0)))
  (let ((observe (compiled-model-observables (integrator-model it))))
    (when observe
      (setf (integrator-phase it) :observables)
      (fill (integrator-obs it) 0d0)
      (funcall (the function observe) (integrator-y-mech it) (integrator-aux it)
               (integrator-params it) (integrator-obs it))))
  (neural-substep it)
  (mech-substep it)
  (check-step it)
  (finish-step it)
  it)

;;; Прогон с перезапусками

(defun arithmetic-cause (e)
  (typecase e
    (division-by-zero :division-by-zero)
    (floating-point-overflow :overflow)
    (floating-point-invalid-operation :invalid)
    (t :arithmetic)))

(defun record-restart (it name condition)
  (push (list (integrator-step it) name
              (if (typep condition 'numerical-blowup) (numerical-blowup-cause condition) condition))
        (integrator-%restarts it))
  (log-event :warn 'solver "шаг ~D: ~(~A~) после ~A" (integrator-step it) name condition))

(defun clamp-state (it)
  "Заменить нечисловые значения значениями начала шага."
  (loop for (now . before) in (list (cons (integrator-y-mech it) (integrator-y-mech0 it))
                                    (cons (integrator-y-neural it) (integrator-y-neural0 it))
                                    (cons (integrator-aux it) (integrator-aux0 it)))
        do (dotimes (i (length now))
             (when (non-finite-p (aref now i))
               (setf (aref now i) (aref before i))))))

(defun run-steps (it n &key (policy #'never-policy))
  "Сделать N шагов. При численном сбое POLICY (функция от условия) выбирает
перезапуск: halve-timestep, rollback-to-checkpoint, clamp-and-continue или
abort-run; NIL — сбой уходит вызывающему, и перезапуски остаются доступны
ему самому (в REPL — человеку). Возвращает статус: :done или :aborted."
  (let ((target (+ (integrator-step it) n))
        (current nil))
    (setf (integrator-status it) :running)
    (loop
      (when (>= (integrator-step it) target)
        (return (setf (integrator-status it) :done)))
      (restart-case
          (handler-bind
              ((numerical-error
                 (lambda (c)
                   (setf current c)
                   (let ((choice (funcall policy c)))
                     (when choice
                       (let ((restart (find-restart choice c)))
                         (when restart (invoke-restart restart))))))))
            (handler-bind
                ((arithmetic-error
                   (lambda (e)
                     (blowup it (arithmetic-cause e) (integrator-phase it)))))
              (loop while (< (integrator-step it) target)
                    do (step! it))))
        (halve-timestep ()
          :report "Откатить шаг и уменьшить h вдвое до конца прогона."
          (record-restart it 'halve-timestep current)
          (return-to-step-start it)
          (setf (integrator-h it) (* 0.5d0 (integrator-h it))
                (integrator-factor-valid it) nil)
          (push (cons (integrator-step it) (integrator-h it)) (integrator-%dt-history it)))
        (rollback-to-checkpoint ()
          :report "Вернуться к последней контрольной точке и продолжить с прежним h."
          (record-restart it 'rollback-to-checkpoint current)
          (restore-checkpoint it :keep-h t))
        (clamp-and-continue ()
          :report "Заменить нечисловые значения и продолжить; прогон помечается недостоверным."
          (record-restart it 'clamp-and-continue current)
          (clamp-state it)
          (setf (integrator-unreliable-p it) t)
          (finish-step it))
        (abort-run ()
          :report "Завершить прогон."
          (record-restart it 'abort-run current)
          (return (setf (integrator-status it) :aborted)))))))

;;;; src/runtime/bench.lisp — замеры шага.
;;;;
;;;; (benchmark-step model :steps 10000) — время шага и его доли: наблюдаемые,
;;;; нервно-мышечная часть, силы, сборка матрицы, LU. Шаг целиком меряется
;;;; прогоном интегратора; доли — вызовами функций модели на его текущем
;;;; состоянии с теми же буферами, без изменения состояния.
;;;; (profile-run model :steps 10000) — статистический профиль sb-sprof.
;;;; (write-benchmark result) — запись в bench/ с версией кода.

(in-package #:worm.runtime)

(defun make-model-integrator (model &key params (h 1d-4) (neural-every 10) (checkpoint-every 1000))
  "Интегратор для модели, собранной WORM.CODEGEN:COMPILE-MODEL, из её
начального состояния."
  (multiple-value-bind (y0 yn0 z0) (worm.codegen:model-initial-state model)
    (worm.solver:make-integrator model
                                 :params (worm.codegen:model-param-vector model params)
                                 :y-mech y0 :y-neural yn0 :z z0
                                 :h h :neural-every neural-every :checkpoint-every checkpoint-every)))

(defun seconds-of (fn count)
  "Среднее время одного вызова FN за COUNT вызовов, с."
  (let ((t0 (get-internal-real-time)))
    (dotimes (k count) (funcall fn))
    (/ (- (get-internal-real-time) t0) internal-time-units-per-second count 1d0)))

(defun benchmark-step (model &key params (steps 2000) (h 1d-4) (warmup 200) (calls 500))
  "Время шага модели и его доли: plist."
  (let* ((it (make-model-integrator model :params params :h h))
         (p (worm.solver:integrator-params it))
         (n (compiled-model-n-mech model))
         (kl (compiled-model-lower-band model)) (ku (compiled-model-upper-band model))
         (nn (compiled-model-n-neural model))
         (z (make-dvec 0))
         (clock (dvec-from (list 0 h 0)))
         (obs (make-dvec (compiled-model-n-observables model)))
         (syn (make-dvec (compiled-model-n-syn model)))
         (g (make-dvec n)) (ab (make-band n kl ku)) (ipiv (make-ivec n))
         (f (make-dvec nn)) (jac (make-dmat nn nn)) (yn (make-dvec nn)))
    (worm.solver:run-steps it warmup)
    (let* ((t0 (get-internal-real-time))
           (_ (worm.solver:run-steps it steps))
           (step (/ (- (get-internal-real-time) t0) internal-time-units-per-second steps 1d0))
           (y (worm.solver:integrator-y-mech it))
           (aux (worm.solver:integrator-aux it)))
      (declare (ignore _))
      (replace yn (worm.solver:integrator-y-neural it))
      (setf (aref clock 0) (worm.solver:integrator-time it))
      (flet ((call (fn &rest args) (if fn (lambda () (apply fn args)) (lambda () nil))))
        (let* ((observables (seconds-of (call (compiled-model-observables model) y aux p obs) calls))
               (rhs (seconds-of (call (compiled-model-neural-rhs model) yn obs z p clock f) calls))
               (jacobian (seconds-of (call (compiled-model-neural-jacobian model) yn obs z p clock jac) calls))
               (output (seconds-of (call (compiled-model-neural-output model) yn obs z p clock syn) calls))
               (forces (seconds-of (call (compiled-model-mech-forces model) y syn p g) calls))
               (matrix (seconds-of (call (compiled-model-mech-matrix model) y aux syn p clock ab) calls))
               (lu (let ((ab0 (make-band n kl ku)) (g0 (make-dvec n)))
                     (funcall (compiled-model-mech-matrix model) y aux syn p clock ab0)
                     (funcall (compiled-model-mech-forces model) y syn p g0)
                     (seconds-of (lambda ()
                                   (replace ab ab0) (replace g g0)
                                   (worm.solver:band-factor! ab n kl ku ipiv)
                                   (worm.solver:band-solve! ab n kl ku ipiv g))
                                 calls)))
               (copy (seconds-of (lambda () (replace ab ab) (replace g g)) calls))
               (neural (+ rhs output (/ jacobian (worm.solver:integrator-neural-every it)))))
          (list :model (compiled-model-name model) :n-mech n :band (list kl ku) :n-neural nn
                :h h :steps steps
                :step-ms (* 1d3 step)
                :parts-ms (list :observables (* 1d3 observables) :neural (* 1d3 neural)
                                :forces (* 1d3 forces) :matrix (* 1d3 matrix)
                                :lu (* 1d3 (max 0d0 (- lu copy)))
                                :other (* 1d3 (- step observables neural forces matrix (max 0d0 (- lu copy)))))
                :simulated-seconds-per-second (/ h step)))))))

(defun profile-run (model &key params (steps 2000) (h 1d-4) (stream *standard-output*))
  "Статистический профиль STEPS шагов модели (sb-sprof, плоский отчёт)."
  (require :sb-sprof)
  (let ((it (make-model-integrator model :params params :h h)))
    (worm.solver:run-steps it 50)
    (funcall (intern "PROFILE-CALL-COUNTS" :sb-sprof) "WORM.CODEGEN")
    (eval `(,(intern "WITH-PROFILING" :sb-sprof) (:max-samples 100000 :sample-interval 0.0005 :report :flat)
            (worm.solver:run-steps ,it ,steps)))
    (finish-output stream)
    (values)))

(defun write-benchmark (results &key (directory (merge-pathnames "bench/" (project-root))) note)
  "Записать замеры RESULTS (список plist) в bench/<дата>-<ревизия>.sexp."
  (multiple-value-bind (s mi h d mo y) (decode-universal-time (get-universal-time) 0)
    (declare (ignore s))
    (let* ((rev (subseq (or (git-revision) "unknown") 0 (min 7 (length (or (git-revision) "unknown")))))
           (path (merge-pathnames (format nil "~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D-~A.sexp" y mo d h mi rev) directory)))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
        (with-standard-io-syntax
          (let ((*print-pretty* t) (*print-right-margin* 100) (*print-readably* nil)
                (*read-default-float-format* 'double-float))
            (format out ";;;; Замер шага: ~A, ~A, ~A~@[~%;;;; ~A~]~%" (or (git-revision) "unknown")
                    (lisp-implementation-version) (machine-type) note)
            (prin1 (list :revision (git-revision) :sbcl (coerce (lisp-implementation-version) 'simple-string)
                         :machine (machine-instance) :results results)
                   out)
            (terpri out))))
      path)))

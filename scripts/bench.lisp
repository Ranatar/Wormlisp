;;;; scripts/bench.lisp — замер шага моделей на языке модели и ручной модели.
;;;;
;;;; make bench — результаты в bench/<дата>-<ревизия>.sexp и на экран.

(asdf:load-system "worm/runtime")
(asdf:load-system "worm/oracle")

(defpackage #:worm.bench (:use #:cl))
(in-package #:worm.bench)

(defun oracle-step-ms (params &key muscles (steps 1000) (h 5d-4))
  (let* ((set (worm.params:load-param-set params))
         (body (worm.oracle:build-body :params set :muscles muscles :ramp 0.5d0
                                       :amplitude (if muscles 1d0 15d0)
                                       :frequency (worm.params:param-value set :gen-frequency)
                                       :wavelength (worm.params:param-value set :gen-wavelength)))
         (it (worm.oracle:make-body-integrator body :h h)))
    (worm.solver:run-steps it 100)
    (let ((t0 (get-internal-real-time)))
      (worm.solver:run-steps it steps)
      (* 1d3 (/ (- (get-internal-real-time) t0) internal-time-units-per-second steps)))))

(let ((results '()))
  (dolist (case '(("water-v1" worm.components:worm-3d t) ("agar-a-v1" worm.components:worm-3d t)
                  ("water-v1" worm.components:worm-3d-kinematic nil)))
    (destructuring-bind (params model muscles) case
      (let* ((t0 (get-internal-real-time))
             (compiled (worm.codegen:compile-model model :params params))
             (build (/ (- (get-internal-real-time) t0) internal-time-units-per-second 1d0))
             (r (worm.runtime:benchmark-step compiled :steps 2000 :h (if (string= params "agar-a-v1") 1d-3 1d-4)))
             (oracle (oracle-step-ms params :muscles muscles :h (if (string= params "agar-a-v1") 1d-3 1d-4))))
        (setf r (append r (list :params params :build-seconds build :oracle-step-ms oracle)))
        (push r results)
        (format t "~&~(~A~), ~A: шаг ~,2F мс (ручная модель ~,2F), сборка ~,1F с~%  доли, мс: ~{~(~A~) ~,3F~^, ~}~%  ~,3F с модели за секунду счёта~%"
                model params (getf r :step-ms) oracle build (getf r :parts-ms) (getf r :simulated-seconds-per-second))
        (finish-output))))
  (format t "~&Записано: ~A~%" (worm.runtime:write-benchmark (nreverse results))))

;;;; tests/runtime.lisp — прогон, поток симуляции, команды (этап 8).
;;;;
;;;; Условия готовности: каждая команда из таблицы «Потоки» покрыта тестом;
;;;; сбой ставит прогон на паузу, а (problem) и (choose …) работают;
;;;; set-param на ходу меняет поведение, параметры без живой правки
;;;; отвергаются с объяснением; rebuild переносит совместимое состояние;
;;;; разрыв терминала посреди прогона не останавливает счёт (здесь — поток
;;;; вывода, закрытый под REPL; через tmux — в tests/runtime-repl.lisp).
;;;;
;;;; Модель — worm-3d на агаре, 8 колец: шаг около миллисекунды.

(in-package #:worm.tests)

(def-suite runtime :in worm :description "worm.runtime: прогон, команды, сбои, пересборка.")
(in-suite runtime)

(defvar *runtime-models* (make-hash-table :test 'equal))

(defun runtime-model (&key (name 'worm.components:worm-3d) (rings 8) (params "agar-a-v1"))
  (let ((key (list name rings params)))
    (or (gethash key *runtime-models*)
        (setf (gethash key *runtime-models*)
              (worm.codegen:compile-model name :params params :rings rings)))))

(defun runtime-dir (name)
  (let ((dir (merge-pathnames (format nil "cache/test-runtime/~A/" name) (worm.core:project-root))))
    (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)
    dir))

(defmacro with-test-run ((var name &rest keys &key (model '(runtime-model)) &allow-other-keys) &body body)
  `(let ((,var (worm.runtime:start-run ,model ,@(alexandria:remove-from-plist keys :model)
                                              :params "agar-a-v1" :dt 1d-3 :paused t
                                              :directory (runtime-dir ,name))))
     (unwind-protect (progn ,@body)
       (ignore-errors (worm.runtime:stop-run ,var)))))

(defun run-state (run)
  "Копия состояния, снятая в потоке симуляции."
  (worm.runtime:call-in-run
   (lambda (r)
     (let ((it (worm.runtime:run-integrator r)))
       (list (copy-seq (worm.solver:integrator-y-mech it)) (copy-seq (worm.solver:integrator-y-neural it))
             (copy-seq (worm.solver:integrator-aux it)) (worm.solver:integrator-step it))))
   run))

;;; Команды

(test pause-resume-and-step
  (with-test-run (run "pause")
    (is (eq :paused (worm.runtime:run-status run)))
    (is (= 25 (worm.runtime:step-run 25 run)))
    (is (= 25 (worm.runtime:snapshot-step (worm.runtime:snapshot run))))
    (is (= 40 (worm.runtime:step-run 15 run)) "шаги складываются")
    (is (eq :running (worm.runtime:resume run)))
    (sleep 0.3)
    (is (eq :paused (worm.runtime:pause run)))
    (let ((after-pause (worm.runtime:snapshot-step (worm.runtime:snapshot run))))
      (is (> after-pause 40) "в работе шаг растёт: ~D" after-pause)
      (sleep 0.2)
      (is (= after-pause (worm.runtime:snapshot-step (worm.runtime:snapshot run))) "на паузе стоит")
      (is (= after-pause (fourth (run-state run)))))))

(test state-ref-reads-the-snapshot
  (with-test-run (run "state")
    (worm.runtime:step-run 10 run)
    (let ((y (first (run-state run))))
      (is (= (coerce (aref y (+ (* 3 (+ (* 3 8) 2)) 1)) 'single-float)
             (worm.runtime:state-ref :ring 3 :node 2 :axis :y :run run)))
      (is (= 3 (length (worm.runtime:state-ref :ring 3 :node 2 :run run))))
      (is (<= 0 (worm.runtime:state-ref :muscle 10 :run run) 1))
      (is (realp (worm.runtime:state-ref :ring 4 :curvature t :run run))))))

(test set-param-changes-behaviour-live
  ;; Два одинаковых прогона; у одного на шаге 50 амплитуда генератора 0:
  ;; до этого шага они совпадают побитово, после — кальций разный.
  (let ((model (runtime-model)))
    (with-test-run (a "param-a" :model model)
      (with-test-run (b "param-b" :model model)
        (worm.runtime:step-run 50 a) (worm.runtime:step-run 50 b)
        (is (equalp (run-state a) (run-state b)))
        (is (= 0d0 (worm.runtime:set-param :gen-amplitude 0 :run b)))
        (is (= 0d0 (worm.runtime:param :gen-amplitude b)))
        (worm.runtime:step-run 300 a) (worm.runtime:step-run 300 b)
        (let ((ca-a (subseq (second (run-state a)) 0 95)) (ca-b (subseq (second (run-state b)) 0 95)))
          (is (> (reduce #'max ca-a) (* 1.05 (reduce #'max ca-b)))
              "без привода кальций ниже: ~,4F против ~,4F" (reduce #'max ca-b) (reduce #'max ca-a)))
        (multiple-value-bind (v unit) (worm.runtime:param :tau-ca b)
          (is (eq :ms unit)) (is (< 1 v 1000)))
        (worm.runtime:set-param :tau-ca 0.05 :unit :s :run b)
        (is (worm.core:approx= 50d0 (worm.runtime:param :tau-ca b) :rel 1d-9) "единица при правке")))))

(test set-param-rejects-what-cannot-change-live
  (with-test-run (run "param-reject")
    (flet ((message (form)
             (handler-case (progn (funcall form) nil) (worm.core:worm-error (e) (princ-to-string e)))))
      (let ((m (message (lambda () (worm.runtime:set-param :prestress 10 :run run)))))
        (is (and m (search "не правится на ходу" m) (search "rebuild" m)) "~A" m))
      (let ((m (message (lambda () (worm.runtime:set-param :body-length 2 :run run)))))
        (is (and m (search "rebuild" m)) "~A" m))
      (let ((m (message (lambda () (worm.runtime:set-param :k-circ-ratio 1 :run run)))))
        (is (and m (search "не читают напрямую" m)) "~A" m)))
    (signals worm.params:unknown-parameter (worm.runtime:set-param :no-such-thing 1 :run run))
    (signals worm.params:unit-mismatch (worm.runtime:set-param :tau-ca 1 :unit :mm :run run))
    (signals worm.params:out-of-bounds (worm.runtime:set-param :tau-ca -5 :run run))
    (let ((before (worm.runtime:param :tau-ca run)))
      (is (= before (progn (handler-bind ((worm.params:out-of-bounds #'worm.params:keep-old-value))
                             (worm.runtime:set-param :tau-ca -5 :run run))
                           (worm.runtime:param :tau-ca run)))
          "keep-old-value"))))

(test load-params-applies-live-and-lists-rebuild
  (with-test-run (run "load-params")
    (let* ((path (merge-pathnames "cache/test-runtime/params/edited-agar.sexp" (worm.core:project-root))))
      (ensure-directories-exist path)
      ;; набор наследует agar-a-v1: родители ищутся рядом с файлом
      (dolist (f (directory (merge-pathnames "params/*.sexp" (worm.core:project-root))))
        (uiop:copy-file f (merge-pathnames (file-namestring f) path)))
      (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
        (format out "(params edited-agar :extends agar-a-v1~%  (tau-ca 60 :ms)~%  (gen-frequency 0.4 :hz)~%  (prestress 20 :kpa))~%"))
      (let ((r (worm.runtime:load-params path run)))
        (is (equal '(:gen-frequency :tau-ca) (sort (mapcar #'first (getf r :applied)) #'string< :key #'symbol-name)))
        (is (equal '(:prestress) (getf r :needs-rebuild)))
        (is (worm.core:approx= 60d0 (worm.runtime:param :tau-ca run) :rel 1d-9))
        (is (worm.core:approx= 0.4d0 (worm.runtime:param :gen-frequency run) :rel 1d-9))))))

(test checkpoint-and-rollback
  (with-test-run (run "checkpoint" :memory-checkpoint-every 0.05d0)
    (worm.runtime:step-run 200 run)
    (let ((path (worm.runtime:save-checkpoint run))
          (at-200 (run-state run)))
      (is (probe-file path))
      (worm.runtime:step-run 100 run)
      (let ((at-300 (run-state run)))
        ;; с диска: то же продолжение побитово
        (is (= 200 (worm.runtime:rollback path run)))
        (is (equalp at-200 (run-state run)) "состояние точки на диске")
        (worm.runtime:step-run 100 run)
        (is (equalp at-300 (run-state run)) "продолжение с диска совпадает побитово")
        ;; из кольца в памяти: точки каждые 50 шагов, вторая с конца — 250
        (is (= 250 (worm.runtime:rollback 1 run)))
        (is (= 250 (worm.runtime:snapshot-step (worm.runtime:snapshot run))))))))

(test rollback-keeps-later-parameter-edits
  ;; Архитектура, «Контрольные точки»: откат восстанавливает состояние, но не
  ;; параметры — правка после снимка остаётся; :params t откатывает и их.
  (with-test-run (run "rollback-params" :memory-checkpoint-every 0.05d0)
    (worm.runtime:step-run 100 run)
    (let ((path (worm.runtime:save-checkpoint run))
          (before (worm.runtime:param :tau-ca run)))
      (worm.runtime:set-param :tau-ca 60 :run run)
      (worm.runtime:step-run 20 run)
      (is (= 100 (worm.runtime:rollback 0 run)))
      (is (worm.core:approx= 60d0 (worm.runtime:param :tau-ca run) :rel 1d-12) "правка осталась")
      (is (= 100 (worm.runtime:rollback 0 :params t :run run)))
      (is (worm.core:approx= before (worm.runtime:param :tau-ca run) :rel 1d-12) "с :params t — значение точки")
      (worm.runtime:set-param :tau-ca 60 :run run)
      (is (= 100 (handler-bind ((warning #'muffle-warning)) (worm.runtime:rollback path run))))
      (is (worm.core:approx= 60d0 (worm.runtime:param :tau-ca run) :rel 1d-12) "с диска правка тоже остаётся")
      (is (search "отличаются от текущих"
                  (alexandria:read-file-into-string (merge-pathnames "log.txt" (worm.runtime:run-directory run)))))
      (is (= 100 (worm.runtime:rollback path run :params t)))
      (is (worm.core:approx= before (worm.runtime:param :tau-ca run) :rel 1d-12)))))

(test disk-checkpoints-on-a-schedule
  (with-test-run (run "checkpoint-schedule" :checkpoint-every 0.04d0)
    (worm.runtime:step-run 130 run)
    (is (equal '("step-120.ckpt" "step-40.ckpt" "step-80.ckpt")
               (sort (mapcar #'file-namestring
                             (directory (merge-pathnames "checkpoints/*.ckpt" (worm.runtime:run-directory run))))
                     #'string<)))))

(test recording-on-off-and-to-another-directory
  (with-test-run (run "record" :frames-every 0.01d0)
    (is (eq t (worm.runtime:record t run)))
    (worm.runtime:step-run 100 run)                   ; кадр в начале и каждые 10 шагов
    (is (null (worm.runtime:record nil run)))
    (worm.runtime:step-run 50 run)
    (worm.runtime:record t run)
    (worm.runtime:step-run 20 run)
    (let ((first-dir (worm.runtime:run-directory run))
          (second-dir (runtime-dir "record-2")))
      (worm.runtime:record-to second-dir run)
      (worm.runtime:step-run 30 run)
      (worm.runtime:stop-run run)
      (let ((a (worm.io:read-frames-file (merge-pathnames "frames.bin" first-dir)))
            (b (worm.io:read-frames-file (merge-pathnames "frames.bin" second-dir))))
        (is (= (+ 11 3) (worm.io:frames-file-count a)) "кадров ~D" (worm.io:frames-file-count a))
        (is (= 4 (worm.io:frames-file-count b)) "кадров ~D" (worm.io:frames-file-count b))
        (is (= 200 (worm.io:frame-step (worm.io:frame-ref b 3)))))
      (is (string= "done" (worm.io:read-status first-dir)))
      (is (string= "done" (worm.io:read-status second-dir)))
      (let ((meta (worm.io:read-json-file (merge-pathnames "meta.json" second-dir))))
        (is (equal "worm-3d" (gethash "name" (gethash "model" meta))))
        (is (stringp (gethash "finished" meta)))
        (is (= 4 (gethash "count" (gethash "frames" meta))))))))

(test stop-closes-the-run
  (with-test-run (run "stop" :record t)
    (worm.runtime:step-run 20 run)
    (is (eq :done (worm.runtime:stop-run run)))
    (is (not (worm.platform:thread-alive-p (worm.runtime::run-thread run))))
    (is (string= "done" (worm.io:read-status (worm.runtime:run-directory run))))
    (signals worm.runtime:run-error (worm.runtime:step-run 10 run))
    (is (search "прогон завершён" (alexandria:read-file-into-string
                                    (merge-pathnames "log.txt" (worm.runtime:run-directory run)))))))

;;; Сбои

(defun collapse-a-compartment (run)
  "Стянуть узлы кольца 4 к оси: отсеки 3 и 4 теряют объём."
  (worm.runtime:call-in-run
   (lambda (r)
     (let ((y (worm.solver:integrator-y-mech (worm.runtime:run-integrator r))))
       (loop for k below 8
             for i = (* 3 (+ (* 4 8) k))
             do (setf (aref y (+ i 1)) (* 0.05d0 (aref y (+ i 1)))
                      (aref y (+ i 2)) (* 0.05d0 (aref y (+ i 2)))))))
   run))

(test failure-pauses-and-waits-for-a-choice
  (with-test-run (run "problem")
    (worm.runtime:step-run 100 run)
    (collapse-a-compartment run)
    (worm.runtime:resume run)
    (is (eq :problem (worm.runtime:wait-run :run run :status :problem :timeout 30)))
    (let ((p (worm.runtime:problem run nil)))
      (is (eq :volume-collapse (worm.runtime:problem-cause p)))
      (is (= 100 (worm.runtime:problem-step p)))
      (is (member 'worm.solver:rollback-to-checkpoint (mapcar #'car (worm.runtime:problem-restarts p))))
      (is (search "rollback-to-checkpoint" (with-output-to-string (s) (worm.runtime:problem run s)))))
    (is (eq :problem (worm.runtime:snapshot-status (worm.runtime:snapshot run))))
    ;; пока решения нет, состояние стоит, а команды состояния отложены
    (signals worm.runtime:run-error (worm.runtime:step-run 5 run))
    (signals worm.runtime:run-error (worm.runtime:choose 'no-such-restart run))
    (is (eq :problem (worm.runtime:run-status run)))
    ;; выбор
    (is (eq 'worm.solver:rollback-to-checkpoint (worm.runtime:choose 'rollback-to-checkpoint run)))
    (sleep 0.3)
    (is (null (worm.runtime:run-problem run)))
    (is (eq :paused (worm.runtime:pause run)))
    (is (> (worm.runtime:snapshot-step (worm.runtime:snapshot run)) 100) "после отката счёт идёт дальше")
    (signals worm.runtime:run-error (worm.runtime:choose 'abort-run run))))

(defun poison-state (run)
  "NaN в координате: шаг падает на недопустимой операции."
  (worm.runtime:call-in-run
   (lambda (r) (setf (aref (worm.solver:integrator-y-mech (worm.runtime:run-integrator r)) 40)
                     (let ((inf sb-ext:double-float-positive-infinity))
                       (declare (notinline -))
                       (worm.platform:with-float-traps-masked () (- inf inf)))))
   run))

(test nan-failure-waits-for-a-choice-too
  ;; снимок состояния с NaN не должен ронять поток симуляции
  (with-test-run (run "problem-nan")
    (worm.runtime:step-run 60 run)
    (poison-state run)
    (worm.runtime:resume run)
    (is (eq :problem (worm.runtime:wait-run :run run :status '(:problem :failed) :timeout 30)))
    (is (eq :invalid (worm.runtime:problem-cause (worm.runtime:run-problem run))))
    (is (eq 'worm.solver:rollback-to-checkpoint (worm.runtime:choose 'rollback-to-checkpoint run)))
    (sleep 0.2)
    (worm.runtime:pause run)
    (is (eq :paused (worm.runtime:run-status run)))
    (is (> (worm.runtime:snapshot-step (worm.runtime:snapshot run)) 60))))

(test stop-during-a-problem-and-batch-policy
  (with-test-run (run "problem-stop")
    (worm.runtime:step-run 30 run)
    (collapse-a-compartment run)
    (worm.runtime:resume run)
    (worm.runtime:wait-run :run run :status :problem :timeout 30)
    (worm.runtime:stop-run run)
    (is (eq :failed (worm.runtime:run-status run)))
    (is (string= "failed" (worm.io:read-status (worm.runtime:run-directory run)))))
  ;; пакетная политика: halve-timestep трижды, затем abort-run. NaN в
  ;; состоянии начала шага уменьшением h не лечится
  (with-test-run (run "batch" :policy :batch)
    (worm.runtime:step-run 30 run)
    (poison-state run)
    (worm.runtime:resume run)
    (is (eq :failed (worm.runtime:wait-run :run run :status '(:failed :done) :timeout 30)))
    (let ((restarts (mapcar #'second (worm.solver:integrator-restarts (worm.runtime:run-integrator run)))))
      (is (equal '(worm.solver:halve-timestep worm.solver:halve-timestep worm.solver:halve-timestep worm.solver:abort-run)
                 restarts) "~S" restarts))
    (let ((meta (worm.io:read-json-file (merge-pathnames "meta.json" (worm.runtime:run-directory run)))))
      (is (= 4 (length (gethash "dt" meta))) "h до и после трёх делений"))))

;;; Пересборка

(test rebuild-transfers-compatible-state
  (with-test-run (run "rebuild")
    (worm.runtime:step-run 120 run)
    (worm.runtime:set-param :tau-ca 70 :run run)
    (let ((before (run-state run))
          (r (worm.runtime:rebuild 'worm.components:worm-3d :run run)))
      (is (eq :kept (getf r :body)))
      (is (equal '((:muscles :ca) (:muscles :rate)) (getf r :neural)))
      (is (equalp before (run-state run)) "та же модель: состояние перенесено целиком")
      (is (worm.core:approx= 70d0 (worm.runtime:param :tau-ca run) :rel 1d-9) "живые правки сохраняются")
      (is (= 220 (worm.runtime:step-run 100 run))))
    ;; на кинематику: тело то же, нервно-мышечной части нет
    (let* ((y (first (run-state run)))
           (r (worm.runtime:rebuild 'worm.components:worm-3d-kinematic :run run)))
      (is (eq :kept (getf r :body)))
      (is (null (getf r :neural)))
      (is (equalp y (first (run-state run))))
      (is (eq 'worm.components:worm-3d-kinematic (worm.core:compiled-model-name (worm.runtime:run-model run))))
      (worm.runtime:step-run 20 run))
    ;; обратно на мышцы с набором: кальций с начальных значений, параметры из набора
    (let ((r (worm.runtime:rebuild 'worm.components:worm-3d :params "agar-a-v1" :run run)))
      (is (eq :kept (getf r :body)))
      (is (null (getf r :params-kept)))
      (is (/= 70d0 (worm.runtime:param :tau-ca run))))
    (is (search "пересборка" (alexandria:read-file-into-string
                              (merge-pathnames "log.txt" (worm.runtime:run-directory run)))))))

(test rebuild-with-other-topology-resets-the-body
  (with-test-run (run "rebuild-topology" :model (runtime-model :rings 8))
    (worm.runtime:step-run 50 run)
    (let* ((other (runtime-model :rings 10))
           (r (worm.runtime:call-in-run
               (lambda (rr) (worm.runtime::replace-model rr other (worm.params:load-param-set "agar-a-v1") t))
               run)))
      (is (eq :reset (getf r :body)))
      (is (equal '((:muscles :ca) (:muscles :rate)) (getf r :neural)))
      (is (= (worm.core:compiled-model-n-mech other) (length (first (run-state run))))))))

;;; Терминал

(test run-survives-its-terminal-closing
  ;; Прогон запускают из потока, чей вывод — поток, который потом
  ;; закрывается (как терминал после разрыва SSH); счёт идёт дальше.
  (let* ((model (runtime-model))
         (dir (runtime-dir "terminal"))
         (out (make-string-output-stream))
         (run nil))
    (worm.platform:join-thread
     (worm.platform:make-thread
      (lambda ()
        (let ((*standard-output* out) (*error-output* out) (worm.core:*log-echo* out))
          (setf run (worm.runtime:start-run model :params "agar-a-v1" :dt 1d-3 :directory dir)))))
     :timeout 30)
    (close out)
    (unwind-protect
         (let ((s1 (worm.runtime:snapshot-step (worm.runtime:snapshot run))))
           (sleep 0.5)
           (is (> (worm.runtime:snapshot-step (worm.runtime:snapshot run)) s1) "счёт идёт")
           (is (eq :running (worm.runtime:run-status run))))
      (worm.runtime:stop-run run))))

(test memory-checkpoints-are-a-multiple-of-the-neural-interval
  ;; 0,05 с при h = 2·10⁻³ — 25 шагов; интервал округляется до кратного R = 10
  (with-test-run (run "checkpoint-q" :dt 2d-3)
    (worm.runtime:step-run 60 run)
    (is (every (lambda (s) (zerop (mod s 10)))
               (worm.runtime:call-in-run (lambda (r) (worm.solver:checkpoint-steps (worm.runtime:run-integrator r))) run)))))

;;;; src/runtime/thread.lisp — цикл потока симуляции и команды.
;;;;
;;;; Команды применяются на границе шага в порядке поступления. Пока прогон
;;;; идёт, поток забирает все накопившиеся команды и делает порцию шагов до
;;;; ближайшей границы кадра, снимка или контрольной точки; на паузе — ждёт
;;;; команду. Поток не пишет в терминал: его потоки вывода связаны с пустым
;;;; потоком, а журнал идёт в файлы, поэтому разрыв SSH ему не мешает.

(in-package #:worm.runtime)

(defparameter +chunk-steps+ 50 "Наибольшая порция шагов между проверками почтового ящика.")

(defun run-active-p (run) (member (run-status run) '(:running :stepping)))
(defun run-finished-p (run) (member (run-status run) '(:done :failed)))

(defun set-status (run status)
  (setf (run-status run) status))

;;; Решение о сбое

(defparameter +solver-restarts+
  '(worm.solver:halve-timestep worm.solver:rollback-to-checkpoint worm.solver:clamp-and-continue
    worm.solver:abort-run))

(defun restart-reports (condition)
  (loop for name in +solver-restarts+
        for r = (find-restart name condition)
        when r collect (cons name (princ-to-string r))))

(defun make-problem-of (run condition)
  (let ((it (run-integrator run)))
    (make-problem :condition condition
                  :cause (if (typep condition 'worm.solver:numerical-blowup)
                             (worm.solver:numerical-blowup-cause condition)
                             (type-of condition))
                  :step (worm.solver:integrator-step it) :time (worm.solver:integrator-time it)
                  :h (worm.solver:integrator-h it)
                  :where (and (typep condition 'worm.solver:numerical-blowup)
                              (worm.solver:numerical-blowup-where condition))
                  :restarts (restart-reports condition)
                  :report (princ-to-string condition))))

(defun await-decision (run condition)
  "Интерактивная политика: пауза, описание сбоя в pending-problem и ожидание
(choose …) в почтовом ящике. Возвращает имя перезапуска."
  (let ((problem (make-problem-of run condition))
        (resume-status (if (eq (run-status run) :stepping) :stepping :running)))
    (set-status run :problem)
    (worm.platform:atomic-cell-swap (run-problem-cell run) problem)
    (run-log run "сбой: ~A; ждёт решения (~{~(~A~)~^, ~})" (problem-report problem)
             (mapcar #'car (problem-restarts problem)))
    (publish-snapshot run)
    (unwind-protect
     (loop
      (let ((cmd (worm.platform:mailbox-receive (run-mailbox run))))
        (destructuring-bind (op promise &rest args) cmd
          (case op
            (:choose
             (let* ((name (first args))
                    (restart (find name (problem-restarts problem) :key #'car :test #'string-equal-symbol)))
               (if restart
                   (progn
                     (worm.platform:atomic-cell-swap (run-problem-cell run) nil)
                     (set-status run resume-status)
                     (run-log run "решение: ~(~A~)" (car restart))
                     (fulfill promise (car restart))
                     (return (car restart)))
                   (reject promise (make-condition 'run-error
                                                   :message (format nil "перезапуска ~(~A~) нет; доступны ~{~(~A~)~^, ~}"
                                                                    name (mapcar #'car (problem-restarts problem))))))))
            (:debug
             ;; терминал REPL передаётся командой: свой вывод поток держит пустым
             (let ((term (or (first args) *terminal-io*)))
               (fulfill promise t)
               (run-log run "отладчик в потоке симуляции")
               (let ((*debugger-hook* nil) (sb-ext:*invoke-debugger-hook* nil)
                     (*terminal-io* term) (*debug-io* term) (*query-io* term)
                     (*standard-output* term) (*error-output* term))
                 (invoke-debugger condition))))
            (:stop
             (worm.platform:atomic-cell-swap (run-problem-cell run) nil)
             (run-log run "остановка во время сбоя")
             ;; abort-run завершит run-steps; команда :stop пойдёт следующей
             (worm.platform:mailbox-send (run-mailbox run) cmd)
             (return 'worm.solver:abort-run))
            (t (handle-command run cmd :problem t))))))
      ;; выход из ожидания — выбором, остановкой или перезапуском из отладчика
      (when (eq (worm.platform:atomic-cell-value (run-problem-cell run)) problem)
        (worm.platform:atomic-cell-swap (run-problem-cell run) nil))
      (when (eq (run-status run) :problem)
        (set-status run resume-status)))))

(defun string-equal-symbol (a b)
  (string-equal (if (symbolp a) (symbol-name a) a) (symbol-name b)))

(defun step-policy (run)
  (let ((policy (run-policy run)))
    (cond ((functionp policy) policy)
          ((eq policy :batch) (worm.solver:make-batch-policy))
          (t (lambda (c) (await-decision run c))))))

;;; Команды

(defun require-not-problem (problem op)
  (when problem
    (error 'run-error :message (format nil "прогон ждёт решения о сбое — (problem), (choose …); команда ~(~S~) отложена до решения" op))))

(defun apply-param (run name index value &optional (source :repl))
  (let* ((p (worm.solver:integrator-params (run-integrator run)))
         (old (aref p index)))
    (setf (aref p index) value)
    (worm.solver:invalidate-factorization (run-integrator run))
    (push (list (worm.solver:integrator-step (run-integrator run)) name old value source) (run-param-changes run))
    (let ((unit (worm.params:parameter-spec-unit (worm.params:find-parameter name))))
      (flet ((shown (x) (if unit (worm.params:from-internal x unit) x)))
        (run-log run "~(~A~): ~,6G → ~,6G~@[ ~(~A~)~]" name (shown old) (shown value) unit)))
    (list old value)))

(defun handle-command (run cmd &key problem)
  "Выполнить команду CMD = (операция обещание аргументы…)."
  (destructuring-bind (op promise &rest args) cmd
    (handler-case
        (ecase op
          (:pause (unless (or problem (run-finished-p run)) (set-status run :paused)) (publish-snapshot run) (fulfill promise (run-status run)))
          (:resume (require-not-problem problem op) (set-status run :running) (fulfill promise :running))
          (:step
           (require-not-problem problem op)
           (setf (run-steps-left run) (first args) (run-step-promise run) promise)
           (set-status run :stepping)
           (when (<= (first args) 0) (finish-stepping run)))
          (:set-param (fulfill promise (apply #'apply-param run args)))
          (:apply-params
           (destructuring-bind (changes set) args
             (when set (setf (run-param-set run) set))
             (fulfill promise (loop for (name index value) in changes
                                    collect (cons name (apply-param run name index value))))
             (when (run-directory run) (write-run-config run))))
          (:checkpoint (fulfill promise (write-disk-checkpoint run)))
          (:rollback
           (require-not-problem problem op)
           (let ((target (first args)))
             (if (or (pathnamep target) (stringp target))
                 (load-disk-checkpoint run target)
                 (progn (worm.solver:restore-checkpoint (run-integrator run) :back target :params t)
                        (run-log run "откат к точке ~D, шаг ~D" target (worm.solver:integrator-step (run-integrator run))))))
           (when (run-writer run) (write-current-frame run))
           (publish-snapshot run)
           (fulfill promise (worm.solver:integrator-step (run-integrator run))))
          (:record
           (let ((on (first args)))
             (setf (run-record-p run) on)
             (cond ((and on (run-writer run)) (write-current-frame run))
                   (on (open-writer run))
                   ((run-writer run) (write-run-meta run)))
             (run-log run "запись кадров ~:[выключена~;включена~]" on)
             (fulfill promise on)))
          (:record-to
           (let ((dir (uiop:ensure-directory-pathname (first args))))
             (close-writer run)
             (run-log run "запись переходит в ~A" (namestring dir))
             (worm.io:write-status (run-directory run) "done")
             (open-run-directory run dir)
             (setf (run-record-p run) t)
             (open-writer run)
             (fulfill promise dir)))
          (:replace
           (require-not-problem problem op)
           (fulfill promise (apply #'replace-model run args)))
          (:call (fulfill promise (funcall (first args) run)))
          ((:choose :debug)
           (error 'run-error :message "сбоя, ждущего решения, нет"))
          (:stop
           (setf (run-steps-left run) 0)
           (finish-run run (if (eq (run-status run) :failed) :failed :done))
           (fulfill promise (run-status run))))
      (error (e)
        (run-log run "команда ~(~S~) не выполнена: ~A" op e)
        (reject promise e)))))

(defun finish-stepping (run)
  (set-status run :paused)
  (publish-snapshot run)
  (let ((p (run-step-promise run)))
    (setf (run-step-promise run) nil)
    (fulfill p (worm.solver:integrator-step (run-integrator run)))))

(defun finish-run (run status)
  "Закрыть файлы, записать итог; статус :done или :failed."
  (unless (run-finished-p run)
    (set-status run status)
    (close-writer run)
    (when (run-directory run)
      (write-run-meta run :finished (iso-utc))
      (worm.io:write-status (run-directory run) (string-downcase status)))
    (run-log run "прогон завершён: ~(~A~)" status)
    (publish-snapshot run)
    (when (run-step-promise run)
      (fulfill (run-step-promise run) (worm.solver:integrator-step (run-integrator run)))
      (setf (run-step-promise run) nil))
    (when (run-log-stream run)
      (close (run-log-stream run))
      (setf (run-log-stream run) nil))))

;;; Порция шагов

(defun log-new-restarts (run)
  (let ((all (worm.solver:integrator-restarts (run-integrator run))))
    (loop for (step name cause) in (nthcdr (run-restarts-logged run) all)
          do (run-log run "перезапуск ~(~A~) на шаге ~D после ~(~A~)~@[, h = ~,2E с~]" name step cause
                      (and (eq name 'worm.solver:halve-timestep) (worm.solver:integrator-h (run-integrator run)))))
    (setf (run-restarts-logged run) (length all))))

(defun steps-to-boundary (step every)
  (if (plusp every) (- every (mod step every)) most-positive-fixnum))

(defun after-steps (run)
  (let ((step (worm.solver:integrator-step (run-integrator run))))
    (when (and (run-record-p run) (run-writer run) (zerop (mod step (run-frame-every run))))
      (write-current-frame run))
    (when (and (plusp (run-checkpoint-every run)) (zerop (mod step (run-checkpoint-every run))))
      (write-disk-checkpoint run))
    (when (zerop (mod step (run-snapshot-every run)))
      (publish-snapshot run))))

(defun run-chunk (run policy)
  (let* ((it (run-integrator run))
         (step (worm.solver:integrator-step it))
         (n (min +chunk-steps+
                 (steps-to-boundary step (if (run-record-p run) (run-frame-every run) 0))
                 (steps-to-boundary step (run-snapshot-every run))
                 (steps-to-boundary step (run-checkpoint-every run))
                 (if (eq (run-status run) :stepping) (run-steps-left run) most-positive-fixnum))))
    (let ((result (worm.solver:run-steps it n :policy policy)))
      (when (eq (run-status run) :stepping)
        (decf (run-steps-left run) (- (worm.solver:integrator-step it) step)))
      (log-new-restarts run)
      (after-steps run)
      (cond ((eq result :aborted)
             (run-log run "прогон прерван перезапуском abort-run")
             (finish-run run :failed))
            ((and (eq (run-status run) :stepping) (<= (run-steps-left run) 0))
             (finish-stepping run))))))

(defun simulation-loop (run)
  (let ((policy (step-policy run))
        (mailbox (run-mailbox run)))
    (loop
      (when (run-finished-p run)
        ;; после завершения отвечаем на запоздавшие команды и выходим
        (loop (multiple-value-bind (cmd ok) (worm.platform:mailbox-receive-no-hang mailbox)
                (cond ((not ok) (return))
                      ((eq (first cmd) :stop) (fulfill (second cmd) (run-status run)))
                      (t (reject (second cmd) (make-condition 'run-error :message "прогон завершён"))))))
        (return))
      (if (run-active-p run)
          (progn
            (loop (multiple-value-bind (cmd ok) (worm.platform:mailbox-receive-no-hang mailbox)
                    (unless ok (return))
                    (handle-command run cmd)))
            (when (run-active-p run)
              (run-chunk run policy)))
          (handle-command run (worm.platform:mailbox-receive mailbox))))))

(defun start-simulation-thread (run)
  (setf (run-thread run)
        (worm.platform:make-thread
         (lambda ()
           (let* ((sink (make-broadcast-stream))
                  (*standard-output* sink) (*error-output* sink) (*trace-output* sink)
                  (*terminal-io* (make-two-way-stream (make-concatenated-stream) sink))
                  (*query-io* *terminal-io*) (*debug-io* *terminal-io*)
                  (*log-echo* nil))
             (handler-case (simulation-loop run)
               (error (e)
                 (ignore-errors (run-log run "поток симуляции упал: ~A" e))
                 (ignore-errors (finish-run run :failed))))))
         :name (format nil "worm run ~A" (run-id run)))))

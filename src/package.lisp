;;;; src/package.lisp
;;;;
;;;; Фасадный пакет WORM — то, чем пользуются из REPL. Пакет WORM-USER —
;;;; рабочий пакет терминального REPL (scripts/repl.sh переходит в него).

(defpackage #:worm
  (:use #:cl)
  (:import-from #:worm.server #:serve #:stop-serving #:serving-p)
  (:import-from #:worm.runtime
   #:build-model #:start-run #:stop-run #:wait-run #:list-runs #:*current-run* #:pause #:resume #:step-run #:call-in-run #:set-param #:param #:load-params #:snapshot #:state-ref #:save-checkpoint #:rollback #:record #:record-to #:problem #:choose #:debug-problem #:rebuild #:benchmark-step #:profile-run)
  (:export #:serve
           #:stop-serving
           #:serving-p
           #:about
           #:run-sweep
           #:build-model #:start-run #:stop-run #:wait-run #:list-runs #:*current-run* #:pause #:resume #:step-run #:call-in-run #:set-param #:param #:load-params #:snapshot #:state-ref #:save-checkpoint #:rollback #:record #:record-to #:problem #:choose #:debug-problem #:rebuild #:benchmark-step #:profile-run))

(in-package #:worm)

(defun about (&optional (stream *standard-output*))
  "Напечатать сведения о процессе: реализация, ревизия, память, ядра, сервер."
  (let ((rev (worm.core:git-revision)))
    (format stream "~&Wormlisp ~A~%  ~A~%  ревизия ~A~%  память ~D МБ, ядер ~D~%  сервер ~A~%"
            (asdf:component-version (asdf:find-system "worm"))
            (worm.platform:implementation-description)
            (or rev "неизвестна")
            (worm.platform:dynamic-space-megabytes)
            (worm.platform:processor-count)
            (if (serving-p) "запущен" "не запущен")))
  (values))

;;; Пакетные прогоны: фасад связывает worm.batch (слой 6) с метриками
;;; worm.analysis (тот же слой): рабочий пишет metrics.csv, оркестратор —
;;; сводку развёртки.

(defun batch-worker (job-path)
  "Точка входа рабочего процесса (worm.batch:orchestrate запускает её в
отдельном SBCL)."
  (worm.batch:worker-main job-path
                          :after (lambda (dir job)
                                   (declare (ignore job))
                                   (worm.analysis:write-run-metrics dir))))

(defun sweep-summary (result)
  "summary.csv развёртки по итогу ORCHESTRATE; путь."
  (let ((rows (loop for job in (getf result :jobs)
                    for id = (getf job :id)
                    for status = (worm.batch:job-status id)
                    collect (list :id id :status (or status "missing") :point (getf job :point)
                                  :metrics (and (equal status "done")
                                                (ignore-errors
                                                 (worm.analysis:run-gait (worm.batch:run-directory id)
                                                                         :from (getf job :metrics-from))))))))
    (worm.analysis:write-sweep-summary
     (merge-pathnames (format nil "runs/sweeps/~A/summary.csv" (getf result :sweep)) (worm.core:project-root))
     rows)))

(defun run-sweep (path &key workers (wait t))
  "Посчитать развёртку из файла PATH (sweeps/*.sexp) рабочими процессами и
записать сводку runs/sweeps/<имя>/summary.csv. Возвращает итог оркестратора.
WAIT NIL — запустить оркестратор отдельным процессом (scripts/run-sweep.sh) и
сразу вернуть его; ход — в runs/sweeps/<имя>/orchestrator.log."
  (unless wait
    (let* ((name (worm.batch:sweep-name (worm.batch:load-sweep path)))
           (log (merge-pathnames (format nil "runs/sweeps/~A/orchestrator.log" name) (worm.core:project-root))))
      (ensure-directories-exist log)
      (format t "~&Развёртка ~A считается в фоне; ход — ~A~%" name (namestring log))
      (return-from run-sweep
        (uiop:launch-program (list (namestring (merge-pathnames "scripts/run-sweep.sh" (worm.core:project-root)))
                                   (namestring (worm.batch::project-path path)) (if workers (princ-to-string workers) "nil"))
                             :output log :error-output :output :if-output-exists :supersede))))
  (let ((result (worm.batch:orchestrate path :workers workers)))
    (setf (getf result :summary) (sweep-summary result))
    (format t "~&Готово: ~D посчитано, ~D сбоев, ~D по пределу времени, ~D уже было; сводка ~A~%"
            (getf result :done) (getf result :failed) (getf result :timeout) (getf result :skipped)
            (namestring (getf result :summary)))
    result))

(defpackage #:worm-user
  (:use #:cl #:worm))

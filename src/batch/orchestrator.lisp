;;;; src/batch/orchestrator.lisp — оркестратор развёртки.
;;;;
;;;; Архитектура, «Оркестратор»: развёртка раскрывается в файлы заданий,
;;;; готовые (status done) пропускаются, fasl модели собирается один раз до
;;;; запуска рабочих, рабочих не больше W, превысивший предел задания
;;;; убивается со статусом timeout. Рабочий — отдельный процесс SBCL из
;;;; worm-image.core (или с загрузкой системы, если образа нет).

(in-package #:worm.batch)

(defvar *worker-megabytes* 300
  "Размер рабочего процесса для выбора W, МБ: замер этапа 11 — 115 МБ пиковой
резидентной памяти у прогона на 50 колец с записью кадров — с запасом на
длинные прогоны. W = min(ядра − 1, доступная память / размер рабочего).")

(defvar *worker-heap-megabytes* 1024 "--dynamic-space-size рабочего процесса.")

(defvar *worker-form* "(worm::batch-worker ~S)"
  "Форма, которую выполняет рабочий; ~S — путь к заданию. Точка входа — в
фасадном пакете WORM: он связывает рабочего с метриками worm.analysis.")

(defun auto-workers ()
  (max 1 (min (1- (worm.platform:processor-count))
              (floor (worm.platform:available-megabytes) *worker-megabytes*))))

(defun image-path () (merge-pathnames "worm-image.core" (project-root)))

(defun worker-command (job-path)
  (let ((image (probe-file (image-path))))
    (append (list (sb-ext:native-namestring sb-ext:*runtime-pathname*))
            (when image (list "--core" (sb-ext:native-namestring image)))
            (list "--dynamic-space-size" (princ-to-string *worker-heap-megabytes*)
                  "--noinform" "--non-interactive" "--no-sysinit" "--no-userinit"
                  "--load" (sb-ext:native-namestring (merge-pathnames "scripts/init.lisp" (project-root)))
                  "--eval" "(asdf:load-system \"worm\")"
                  "--eval" (format nil *worker-form* (sb-ext:native-namestring job-path))))))

(defstruct (slot (:copier nil))
  job process started (peak 0.0))

(defun job-finished-p (job)
  (equal (job-status (getf job :id)) "done"))

(defun prepare-model (job)
  "Собрать fasl модели задания один раз до запуска рабочих (кэш codegen)."
  (let ((set (worm.params:derive-param-set
              (worm.params:load-param-set (let ((p (getf job :params))) (if (find #\/ p) (project-path p) p)))
              (format nil "job-~A" (getf job :id)) (getf job :set))))
    (worm.codegen:compile-model (find-model-symbol (getf job :model)) :params set
                                :medium (getf job :medium) :rings (getf job :rings) :cache t)))

(defun mark-timeout (job)
  (let* ((id (getf job :id)) (tmp (tmp-directory id)) (final (run-directory id)))
    (ensure-directories-exist tmp)
    (with-open-file (out (merge-pathnames "log.txt" tmp) :direction :output :if-exists :append
                                                         :if-does-not-exist :create :external-format :utf-8)
      (format out "~A: превышен предел задания ~D с, рабочий остановлен~%" (utc-now) (getf job :timeout)))
    (worm.io:write-status tmp "timeout")
    (uiop:delete-directory-tree final :validate t :if-does-not-exist :ignore)
    (worm.platform:rename-directory tmp final)))

(defun orchestrate (sweep-path &key workers (poll 0.5) (stream *standard-output*) (prepare t))
  "Посчитать развёртку: plist (:sweep :jobs :skipped :done :failed :timeout
:workers :worker-megabytes)."
  (let* ((sweep (load-sweep sweep-path))
         (jobs (expand-sweep sweep))
         (w (let ((spec (or workers (sweep-workers sweep))))
              (if (or (null spec) (eq spec :auto))
                  (auto-workers)
                  (if (and (integerp spec) (plusp spec))
                      spec
                      (batch-error "число рабочих — :auto или целое > 0, а не ~S" spec)))))
         (todo (remove-if #'job-finished-p jobs))
         (skipped (- (length jobs) (length todo)))
         (active '()) (peak 0.0) (counts (list :done 0 :failed 0 :timeout 0)))
    (dolist (job jobs) (write-job job))
    (format stream "~&Развёртка ~A: заданий ~D, готовых ~D, рабочих ~D~%"
            (sweep-name sweep) (length jobs) skipped w)
    (when (and prepare todo)
      ;; разные модели заданий собираются по одному разу
      (let ((seen '()))
        (dolist (job todo)
          (let ((key (list (getf job :model) (getf job :medium) (getf job :rings)
                           (remove-if-not (lambda (e) (member (first e) '(:rf-length :rf-direction :units)))
                                          (getf job :set)))))
            (unless (member key seen :test #'equal)
              (push key seen)
              (handler-case (prepare-model job)
                (error (e) (format stream "~&  модель задания ~A не собрана: ~A~%" (getf job :id) e))))))))
    (flet ((finish (slot status)
             (incf (getf counts (intern (string-upcase status) :keyword) 0))
             (format stream "~&  ~A ~(~A~)~@[ ~{~(~A~)=~,4G~^ ~}~]~%"
                     (getf (slot-job slot) :id) status (apply #'append (getf (slot-job slot) :point)))
             (finish-output stream)))
      (loop
        ;; запуск
        (loop while (and todo (< (length active) w))
              do (let* ((job (pop todo))
                        (out (merge-pathnames (format nil "~A.out" (getf job :id)) (jobs-directory (getf job :sweep)))))
                   (push (make-slot :job job :started (get-internal-real-time)
                                    :process (uiop:launch-program (worker-command (job-path job))
                                                                  :output out :error-output :output
                                                                  :if-output-exists :supersede))
                         active)))
        (when (and (null todo) (null active)) (return))
        (sleep poll)
        ;; надзор
        (dolist (slot (copy-list active))
          (let* ((process (slot-process slot))
                 (pid (uiop:process-info-pid process)))
            (let ((mb (and pid (worm.platform:process-peak-megabytes pid))))
              (when mb (setf (slot-peak slot) (max (slot-peak slot) mb) peak (max peak mb))))
            (cond ((not (uiop:process-alive-p process))
                   (uiop:wait-process process)
                   (setf active (remove slot active))
                   (finish slot (or (job-status (getf (slot-job slot) :id)) "failed")))
                  ((> (/ (- (get-internal-real-time) (slot-started slot)) internal-time-units-per-second)
                      (getf (slot-job slot) :timeout))
                   (uiop:terminate-process process :urgent t)
                   (uiop:wait-process process)
                   (setf active (remove slot active))
                   (mark-timeout (slot-job slot))
                   (finish slot "timeout")))))))
    (list :sweep (sweep-name sweep) :jobs jobs :skipped skipped
          :done (getf counts :done) :failed (getf counts :failed) :timeout (getf counts :timeout)
          :workers w :worker-megabytes peak)))

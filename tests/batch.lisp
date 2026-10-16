;;;; tests/batch.lisp — пакетные прогоны (этап 11).
;;;;
;;;; Быстрая часть: раскрытие развёрток, идентификаторы заданий, рабочий в
;;;; том же процессе и побитовое совпадение с прогоном в REPL. Процессы
;;;; рабочих, повторный запуск, сбой и предел времени — в части make
;;;; test-full внизу файла.

(in-package #:worm.tests)

(def-suite batch :in worm :description "worm.batch: развёртки, задания, рабочие.")
(in-suite batch)

(defun batch-data (name) (merge-pathnames (format nil "tests/data/batch/~A" name) (worm.core:project-root)))

(test sweeps-expand-into-jobs
  (let* ((sweep (worm.batch:load-sweep (merge-pathnames "sweeps/proprio-viscosity.sexp" (worm.core:project-root))))
         (jobs (worm.batch:expand-sweep sweep)))
    (is (= (* 5 2 2) (length jobs)))
    (is (= (length jobs) (length (remove-duplicates (mapcar (lambda (j) (getf j :id)) jobs) :test #'string=)))
        "идентификаторы различны")
    ;; множитель: C_n воды × 10⁴ = 52, C_t × 10⁴ = 33
    (let ((last (find-if (lambda (j) (and (= 10000 (second (assoc :viscosity (getf j :point))))
                                          (= 100 (second (assoc :sr-filter (getf j :point))))
                                          (= 100 (second (assoc :tau-ca (getf j :point))))))
                         jobs)))
      (is (not (null last)))
      (is (worm.core:approx= 52d0 (second (assoc :c-n (getf last :set))) :rel 1d-9))
      (is (worm.core:approx= 33d0 (second (assoc :c-t (getf last :set))) :rel 1d-9))
      (is (= 10d0 (getf last :duration)) "длительность развёртки заменяет базовую"))
    ;; значения геометрической прогрессии
    (is (equal '(1d0 10d0 100d0 1000d0 10000d0)
               (mapcar (lambda (x) (/ (fround (* x 1000)) 1000))
                       (getf (first (worm.batch:sweep-vary sweep)) :values))))))

(test job-ids-depend-only-on-the-configuration
  (let* ((a (worm.batch:expand-sweep (worm.batch:load-sweep (batch-data "tiny-sweep.sexp"))))
         (b (worm.batch:expand-sweep (worm.batch:load-sweep (batch-data "slow-sweep.sexp")))))
    ;; то же значение sr-scale, но другая длительность — другой прогон
    (is (not (string= (getf (second a) :id) (getf (first b) :id))))
    ;; повторное раскрытие даёт те же идентификаторы
    (is (equal (mapcar (lambda (j) (getf j :id)) a)
               (mapcar (lambda (j) (getf j :id)) (worm.batch:expand-sweep (worm.batch:load-sweep (batch-data "tiny-sweep.sexp"))))))
    ;; файл задания читается обратно тем же
    (let ((path (worm.batch:write-job (first a))))
      (is (equalp (first a) (worm.batch:read-job path))))))

(test latin-hypercube-stratifies-every-axis
  (let* ((path (merge-pathnames "cache/test-batch/lhs.sexp" (worm.core:project-root))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "(sweep lhs :base #p\"tests/data/batch/tiny.sexp\"~%  :vary ((sr-scale :linear 1 2 :steps 2) (tau-ca :geometric 50 400 :steps 2))~%  :design (:latin-hypercube :samples 8 :seed 3))~%"))
    (let* ((jobs (worm.batch:expand-sweep (worm.batch:load-sweep path)))
           (s (mapcar (lambda (j) (second (assoc :sr-scale (getf j :point)))) jobs))
           (tau (mapcar (lambda (j) (second (assoc :tau-ca (getf j :point)))) jobs)))
      (is (= 8 (length jobs)))
      ;; в каждом из восьми слоёв по одной точке
      (is (equal '(1 1 1 1 1 1 1 1)
                 (loop for k below 8 collect (count-if (lambda (x) (= k (min 7 (floor (* 8 (- x 1)))))) s))))
      (is (equal '(1 1 1 1 1 1 1 1)
                 (loop for k below 8 collect (count-if (lambda (x) (= k (min 7 (floor (* 8 (log (/ x 50) 8)))))) tau)))))))

(defun final-state (dir)
  "Векторы последней контрольной точки каталога прогона."
  (let* ((files (directory (merge-pathnames "checkpoints/*.ckpt" dir)))
         (last (first (sort files #'> :key (lambda (p) (parse-integer (pathname-name p) :start 5))))))
    (getf (worm.io:read-checkpoint-file last) :vectors)))

(test in-process-job-matches-the-repl-run
  ;; Рабочий без потока и прогон в REPL с потоком — одна и та же модель,
  ;; набор и число шагов: конечное состояние побитово одинаковое.
  (let* ((job (second (worm.batch:expand-sweep (worm.batch:load-sweep (batch-data "tiny-sweep.sexp")))))
         (path (worm.batch:write-job job)))
    (is (equal "done" (worm.batch:run-job path)))
    (let* ((dir (worm.batch:run-directory (getf job :id)))
           (set (worm.params:derive-param-set (worm.params:load-param-set "agar-a-v1") "repl" (getf job :set)))
           (model (worm.codegen:compile-model 'worm.components:worm-3d-proprio :params set :rings 8))
           (run (worm.runtime:start-run model :params set :dt 1d-3 :paused t :directory (runtime-dir "batch-repl"))))
      (is (probe-file (merge-pathnames "frames.bin" dir)))
      (is (equal "done" (worm.io:read-status dir)))
      (unwind-protect
           (progn
             (worm.runtime:step-run 300 run)
             (worm.runtime:save-checkpoint run)
             (is (equalp (final-state dir) (final-state (worm.runtime:run-directory run)))
                 "конечное состояние побитово то же"))
        (worm.runtime:stop-run run)))))

(test bad-job-fails-cleanly-in-process
  (let* ((job (fourth (worm.batch:expand-sweep (worm.batch:load-sweep (batch-data "tiny-sweep.sexp")))))
         (path (worm.batch:write-job job)))
    (is (= -5d0 (second (assoc :sr-scale (getf job :set)))))
    (is (equal "failed" (worm.batch:run-job path)))
    (is (search "не выполнено" (alexandria:read-file-into-string
                                (merge-pathnames "log.txt" (worm.batch:run-directory (getf job :id))))))))

;;; Процессы рабочих (make test-full)

(in-suite physics)

(defun clear-job-runs (sweep-file)
  (dolist (job (worm.batch:expand-sweep (worm.batch:load-sweep (batch-data sweep-file))))
    (uiop:delete-directory-tree (worm.batch:run-directory (getf job :id)) :validate t :if-does-not-exist :ignore)))

(test sweep-runs-in-worker-processes
  (clear-job-runs "tiny-sweep.sexp")
  (let* ((out (make-string-output-stream))
         (result (let ((*standard-output* out)) (worm:run-sweep (batch-data "tiny-sweep.sexp") :workers 2))))
    (is (= 3 (getf result :done)) "~A" (get-output-stream-string out))
    (is (= 1 (getf result :failed)) "сбой одного задания не задевает остальные")
    (is (> (getf result :worker-megabytes) 50) "размер рабочего замерен: ~,0F МБ" (getf result :worker-megabytes))
    (worm.core:log-event :info 'tests "пиковый размер рабочего ~,0F МБ" (getf result :worker-megabytes))
    (let ((summary (alexandria:read-file-into-string (getf result :summary))))
      (is (search "sr_scale" (substitute #\_ #\- summary)))
      (is (= 5 (count #\Newline summary)) "заголовок и четыре строки"))
    ;; бинарно то же, что в процессе: конечное состояние задания совпадает
    ;; с прогоном того же задания в этом процессе
    (let* ((job (second (getf result :jobs)))
           (dir (worm.batch:run-directory (getf job :id)))
           (state (final-state dir)))
      (is (equal "done" (worm.io:read-status dir)))
      (is (probe-file (merge-pathnames "metrics.csv" dir)) "рабочий пишет метрики")
      (let ((path (worm.batch:write-job (list* :id "inprocess-copy" (alexandria:remove-from-plist job :id)))))
        (worm.batch:run-job path)
        (is (equalp state (final-state (worm.batch:run-directory "inprocess-copy")))
            "процесс рабочего и REPL считают побитово одинаково")))
    ;; повторный запуск пропускает готовые
    (let ((again (let ((*standard-output* (make-broadcast-stream)))
                   (worm:run-sweep (batch-data "tiny-sweep.sexp") :workers 2))))
      (is (= 3 (getf again :skipped)))
      (is (= 0 (getf again :done)))
      (is (= 1 (getf again :failed)) "упавшее задание считается снова"))))

(test worker-timeout-kills-the-job
  (clear-job-runs "slow-sweep.sexp")
  (let ((result (let ((*standard-output* (make-broadcast-stream)))
                  (worm.batch:orchestrate (batch-data "slow-sweep.sexp") :workers 1))))
    (is (= 1 (getf result :timeout)))
    (let ((dir (worm.batch:run-directory (getf (first (getf result :jobs)) :id))))
      (is (equal "timeout" (worm.io:read-status dir)))
      (is (search "превышен предел" (alexandria:read-file-into-string (merge-pathnames "log.txt" dir)))))))

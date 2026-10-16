;;;; src/batch/package.lisp
;;;;
;;;; Слой 6. Пакетные прогоны (архитектура, «Пакетные прогоны»): описания
;;;; развёрток и конфигураций, раскрытие в задания, оркестратор процессов и
;;;; точка входа рабочего.

(defpackage #:worm.batch
  (:use #:cl #:worm.core)
  (:export
   #:batch-error
   #:load-config #:load-sweep #:expand-sweep #:sweep-name #:sweep-vary
   #:write-job #:read-job #:job-path #:jobs-directory
   #:run-job #:worker-main #:job-status #:run-directory
   #:orchestrate #:auto-workers #:*worker-megabytes* #:*worker-heap-megabytes*))

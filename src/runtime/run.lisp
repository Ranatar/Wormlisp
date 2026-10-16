;;;; src/runtime/run.lisp — объект прогона и поток симуляции.
;;;;
;;;; Архитектура, «Потоки и интерактивная работа». Состоянием прогона владеет
;;;; один поток — поток симуляции. Остальные пишут команды в его почтовый
;;;; ящик и читают снимки из атомарной ячейки. Команда — список
;;;; (операция обещание аргументы…); синхронные функции REPL ждут ответа на
;;;; обещании, сервер может не ждать.
;;;;
;;;; Сбой шага при интерактивной политике ставит прогон в состояние :problem:
;;;; поток кладёт описание сбоя в ячейку pending-problem и ждёт выбора в
;;;; своём почтовом ящике, оставаясь в динамическом контексте перезапусков
;;;; решателя. (choose …) присылает имя перезапуска, поток вызывает его сам.

(in-package #:worm.runtime)

;;; Обещание

(defstruct (promise (:constructor make-promise ()))
  (semaphore (worm.platform:make-semaphore "worm promise"))
  (done nil)
  (value nil)
  (error nil))

(defun fulfill (promise value)
  (when promise
    (setf (promise-value promise) value (promise-done promise) t)
    (worm.platform:signal-semaphore (promise-semaphore promise))))

(defun reject (promise condition)
  (when promise
    (setf (promise-error promise) condition (promise-done promise) t)
    (worm.platform:signal-semaphore (promise-semaphore promise))))

(define-condition run-error (worm-error) ()
  (:documentation "Команда прогона не выполнена: прогон остановлен, ждёт решения о сбое, неверные аргументы."))

(define-condition run-timeout (run-error) ())

(defun await (promise &key (timeout 60))
  "Ответ на обещание; ошибка потока симуляции переподаётся здесь."
  (unless (worm.platform:wait-on-semaphore (promise-semaphore promise) :timeout timeout)
    (error 'run-timeout :message (format nil "поток симуляции не ответил за ~D с" timeout)))
  (if (promise-error promise)
      (error (promise-error promise))
      (promise-value promise)))

;;; Снимок и сбой

(defstruct (snapshot (:copier nil))
  "Неизменяемый снимок состояния для читателей из других потоков."
  (step 0 :read-only t)
  (time 0d0 :read-only t)
  (h 0d0 :read-only t)
  (status :paused :read-only t)
  (coords nil :read-only t)         ; fvec 3n: x, y, z узлов, мм
  (activations nil :read-only t)    ; fvec m: активации мышц 0…1
  (curvature nil :read-only t)      ; fvec N: кривизна в кольцах, 1/мм
  (rings 0 :read-only t)
  (ring-nodes 0 :read-only t))

(defstruct (problem (:copier nil))
  "Сбой, ждущий решения: условие, где и когда, доступные перезапуски."
  condition cause step time h where restarts report)

(defmethod print-object ((p problem) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~(~A~) на шаге ~D, t = ~,4F с, h = ~,2E с~@[, элемент ~A~]"
            (problem-cause p) (problem-step p) (problem-time p) (problem-h p) (problem-where p))))

;;; Прогон

(defstruct (run (:constructor %make-run) (:copier nil))
  (id "" :type string)
  model                     ; compiled-model
  integrator
  param-set                 ; набор, с которым прогон начат или перезагружен
  (mailbox (worm.platform:make-mailbox "worm run"))
  thread
  (snapshot-cell (worm.platform:make-atomic-cell nil))
  (problem-cell (worm.platform:make-atomic-cell nil))
  (status :paused)          ; :paused :running :stepping :problem :done :failed (пишет только поток симуляции)
  (policy :interactive)     ; :interactive, :batch или функция от условия
  policy-fn                 ; функция политики, созданная один раз на прогон
  (steps-left 0)            ; для :step
  step-promise
  directory                 ; каталог прогона
  (record-p nil)
  writer                    ; frames-writer или NIL
  (frame-every 100)         ; шагов между кадрами
  (snapshot-every 100)      ; шагов между снимками
  (checkpoint-every 0)      ; шагов между точками на диске; 0 — только по команде
  log-stream
  (started "")
  (param-changes '())       ; ((шаг имя старое новое источник) …), новые первыми
  (disk-checkpoints '())    ; пути, новые первыми
  (frames-written 0)        ; кадров в закрытом файле текущего каталога
  (restarts-logged 0)       ; сколько перезапусков решателя уже в log.txt
  ;; буферы кадра и снимка, выделенные один раз
  act-buffer act-fvec curv-buffer centers clock)

(defmethod print-object ((r run) stream)
  (print-unreadable-object (r stream :type t)
    (let ((s (run-snapshot r)))
      (format stream "~A ~(~A~), ~(~A~)~@[, шаг ~D~]~@[, t = ~,4F с~]"
              (run-id r) (compiled-model-name (run-model r)) (run-status r)
              (and s (snapshot-step s)) (and s (snapshot-time s))))))

(defun run-snapshot (run) (worm.platform:atomic-cell-value (run-snapshot-cell run)))
(defun run-problem (run) (worm.platform:atomic-cell-value (run-problem-cell run)))

;;; Журнал прогона

(defun iso-utc ()
  (multiple-value-bind (s mi h d mo y) (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" y mo d h mi s)))

(defun run-log (run control &rest args)
  "Строка в log.txt прогона и в общий журнал."
  (let ((text (apply #'format nil control args))
        (it (run-integrator run)))
    (let ((out (run-log-stream run)))
      (when (and out (open-stream-p out))
        (format out "~A шаг ~D, t = ~,5F: ~A~%" (iso-utc)
                (if it (worm.solver:integrator-step it) 0) (if it (worm.solver:integrator-time it) 0d0) text)
        (finish-output out)))
    (log-event :info 'runtime "~A: ~A" (run-id run) text)))

;;; Снимки и кадры

(defun model-rings (model) (worm.topology:tube-rings (compiled-model-topology model)))
(defun model-ring-nodes (model) (worm.topology:tube-ring-nodes (compiled-model-topology model)))
(defun model-muscle-count (model)
  (let ((a (compiled-model-anatomy model)))
    (if a (length (worm.anatomy:anatomy-muscles a)) 0)))

(defun allocate-buffers (run)
  (let* ((model (run-model run))
         (m (model-muscle-count model))
         (rings (model-rings model)))
    (setf (run-act-buffer run) (make-dvec m)
          (run-act-fvec run) (make-fvec m)
          (run-curv-buffer run) (make-dvec rings)
          (run-centers run) (make-dvec (* 3 rings))
          (run-clock run) (make-dvec 3))))

(defun compute-activations (run)
  "Активации мышц текущего состояния в act-fvec."
  (let* ((it (run-integrator run))
         (clock (run-clock run))
         (out (run-act-buffer run)))
    (setf (aref clock 0) (worm.solver:integrator-time it)
          (aref clock 1) (worm.solver:integrator-h it))
    (if (worm.codegen:model-activations (run-model run) (worm.solver:integrator-y-neural it)
                                        (worm.solver:integrator-obs it) (worm.solver:integrator-params it)
                                        clock out)
        (map-into (run-act-fvec run) (lambda (x) (coerce (max 0d0 (min 1d0 x)) 'single-float)) out)
        (fill (run-act-fvec run) 0f0))
    (run-act-fvec run)))

(defun publish-snapshot (run)
  "Новый снимок в ячейку: единственное место, где поток симуляции выделяет
память, — вне шага. Ловушки плавающей точки здесь выключены: снимок
состояния со сбоем (NaN) показывают как есть, а не роняют поток."
  (worm.platform:with-float-traps-masked () (%publish-snapshot run)))

(defun %publish-snapshot (run)
  (let* ((it (run-integrator run))
         (model (run-model run))
         (y (worm.solver:integrator-y-mech it))
         (rings (model-rings model)) (kk (model-ring-nodes model))
         (curv (run-curv-buffer run)))
    (worm.topology:ring-curvatures y rings kk (run-centers run) curv)
    (worm.platform:atomic-cell-swap
     (run-snapshot-cell run)
     (make-snapshot :step (worm.solver:integrator-step it) :time (worm.solver:integrator-time it)
                    :h (worm.solver:integrator-h it) :status (run-status run)
                    :coords (map 'fvec (lambda (x) (coerce x 'single-float)) y)
                    :activations (copy-seq (compute-activations run))
                    :curvature (map 'fvec (lambda (x) (coerce x 'single-float)) curv)
                    :rings rings :ring-nodes kk))))

(defun write-current-frame (run)
  (worm.platform:with-float-traps-masked () (%write-current-frame run)))

(defun %write-current-frame (run)
  (let ((it (run-integrator run)))
    (worm.io:write-frame (run-writer run) (worm.solver:integrator-step it) (worm.solver:integrator-time it)
                         (worm.solver:integrator-y-mech it) (compute-activations run))))

;;; Каталог прогона

(defun dt-history (it)
  (let ((pairs (worm.solver:integrator-dt-history it)))
    (map 'vector (lambda (p) (worm.io:json-object "fromStep" (car p) "value" (cdr p))) pairs)))

(defun write-run-config (run)
  (let* ((model (run-model run)) (it (run-integrator run)) (set (run-param-set run))
         (path (worm.io:run-file (run-directory run) "config.sexp")))
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create
                              :external-format :utf-8)
      (with-standard-io-syntax
        (let ((*print-case* :downcase) (*print-readably* nil))
          (format out ";;;; Полная конфигурация прогона ~A.~%" (run-id run))
          (pprint `(run :model ,(compiled-model-name model)
                        :model-hash ,(compiled-model-hash model)
                        :params ,(worm.params:param-set-name set)
                        :params-hash ,(worm.params:param-set-hash set)
                        :medium ,(worm.codegen:model-medium model)
                        :rings ,(model-rings model) :ring-nodes ,(model-ring-nodes model)
                        :dt ,(worm.solver:integrator-h it)
                        :frames-every ,(run-frame-every run)
                        :checkpoint-every ,(run-checkpoint-every run)
                        :policy ,(if (functionp (run-policy run)) :custom (run-policy run))
                        :model-params ,(loop for name across (worm.codegen:model-param-names model)
                                             for v across (worm.solver:integrator-params it)
                                             collect (list name v))
                        :internal-values
                        ,(loop for name in (worm.params:param-names set)
                               for v = (worm.params:param-entry-value (worm.params:param-entry-of set name))
                               collect (list name (if (typep v 'dvec) (coerce v 'list) v)))
                        :units (:length :mm :time :s :force :un))
                  out)
          (terpri out))))))

(defun write-run-meta (run &key finished)
  (let* ((model (run-model run)) (it (run-integrator run)) (set (run-param-set run))
         (tube (compiled-model-topology model))
         (anatomy (compiled-model-anatomy model))
         (writer (run-writer run)))
    (worm.io:write-json-file
     (worm.io:run-file (run-directory run) "meta.json")
     (worm.io:json-object
      "format" 1
      "code" (worm.io:json-object "commit" (or (git-revision) 'null) "dirty" 'null
                                  "sbcl" (worm.platform:implementation-description)
                                  "codegen" worm.codegen:+codegen-version+)
      "model" (worm.io:json-object "name" (string-downcase (compiled-model-name model))
                                   "hash" (compiled-model-hash model)
                                   "rings" (worm.topology:tube-rings tube)
                                   "ringNodes" (worm.topology:tube-ring-nodes tube))
      "params" (worm.io:json-object "set" (string-downcase (worm.params:param-set-name set))
                                    "hash" (worm.params:param-set-hash set))
      "units" (worm.io:json-object "length" "mm" "time" "s" "force" "uN")
      "medium" (string-downcase (worm.codegen:model-medium model))
      "dt" (let ((h (dt-history it)))
             (if (plusp (length h)) h
                 (vector (worm.io:json-object "fromStep" 0 "value" (worm.solver:integrator-h it)))))
      "frames" (worm.io:json-object "every" (* (run-frame-every run) (worm.solver:integrator-h it))
                                    "count" (if writer (worm.io:frames-writer-count writer) (run-frames-written run)))
      "restarts" (map 'vector (lambda (r) (format nil "~(~{~A~^ ~}~)" r)) (reverse (worm.solver:integrator-restarts it)))
      "paramChanges" (map 'vector (lambda (c) (destructuring-bind (step name old new &optional source) c
                                                (worm.io:json-object "step" step "name" (string-downcase name)
                                                                     "from" old "to" new
                                                                     "source" (string-downcase (or source :repl)))))
                          (reverse (run-param-changes run)))
      "unreliable" (and (worm.solver:integrator-unreliable-p it) t)
      "started" (run-started run) "finished" (or finished 'null)
      "topology" (worm.io:json-object "rings" (worm.topology:tube-rings tube)
                                      "ringNodes" (worm.topology:tube-ring-nodes tube)
                                      "length" (worm.topology:tube-length tube)
                                      "radiusMax" (worm.topology:tube-radius-max tube))
      "muscles" (if anatomy
                    (map 'vector (lambda (m)
                                   (worm.io:json-object
                                    "name" (string-downcase (worm.anatomy:muscle-name m))
                                    "quadrant" (string-downcase (worm.anatomy:muscle-quadrant m))
                                    "row" (string-downcase (worm.anatomy:muscle-row m))
                                    "from" (worm.anatomy:muscle-from m)
                                    "peak" (worm.anatomy:muscle-peak m)
                                    "to" (worm.anatomy:muscle-to m)))
                         (worm.anatomy:anatomy-muscles anatomy))
                    #())))))

(defun open-run-directory (run directory)
  "Сделать DIRECTORY каталогом прогона: status, config.sexp, meta.json, log.txt."
  (ensure-directories-exist directory)
  (setf (run-directory run) directory (run-frames-written run) 0)
  (worm.io:write-status directory "running")
  (when (run-log-stream run) (close (run-log-stream run)))
  (setf (run-log-stream run)
        (open (worm.io:run-file directory "log.txt") :direction :output :if-exists :append
                                                     :if-does-not-exist :create :external-format :utf-8))
  (write-run-config run)
  (write-run-meta run))

(defun open-writer (run)
  (unless (run-writer run)
    (setf (run-writer run)
          (worm.io:open-frames-writer (worm.io:run-file (run-directory run) "frames.bin")
                                      (worm.topology:tube-node-count (compiled-model-topology (run-model run)))
                                      (model-muscle-count (run-model run))))
    (write-current-frame run)))

(defun close-writer (run)
  (when (run-writer run)
    (worm.io:close-frames-writer (run-writer run))
    (setf (run-frames-written run) (worm.io:frames-writer-count (run-writer run)))
    (write-run-meta run)
    (setf (run-writer run) nil)))

;;; Контрольные точки на диске

(defun layout-hash (model)
  (worm.platform:md5-hex (format nil "~A ~D ~D ~D" (compiled-model-hash model) (compiled-model-n-mech model)
                                 (compiled-model-n-neural model) (compiled-model-n-aux model))))

(defun write-disk-checkpoint (run)
  (let* ((it (run-integrator run))
         (model (run-model run))
         (path (merge-pathnames (format nil "checkpoints/step-~D.ckpt" (worm.solver:integrator-step it))
                                (run-directory run))))
    (worm.io:write-checkpoint-file path :model-hash (compiled-model-hash model) :layout-hash (layout-hash model)
                                        :step (worm.solver:integrator-step it)
                                        :time (worm.solver:integrator-time it)
                                        :h (worm.solver:integrator-h it)
                                        :vectors (list (cons "y-mech" (worm.solver:integrator-y-mech it))
                                                       (cons "y-neural" (worm.solver:integrator-y-neural it))
                                                       (cons "z" (worm.solver:integrator-z it))
                                                       (cons "aux" (worm.solver:integrator-aux it))
                                                       (cons "p" (worm.solver:integrator-params it))))
    (push path (run-disk-checkpoints run))
    (run-log run "контрольная точка ~A" (enough-namestring path (run-directory run)))
    path))

(defun load-disk-checkpoint (run path)
  "Состояние из файла PATH: точка должна быть от той же модели."
  (let* ((model (run-model run))
         (c (worm.io:read-checkpoint-file path :model-hash (compiled-model-hash model)))
         (it (run-integrator run))
         (v (getf c :vectors)))
    (unless (equal (getf c :layout-hash) (layout-hash model))
      (error 'worm.io:checkpoint-mismatch :message (format nil "~A: раскладка состояния другой модели" path)))
    (flet ((put (name target)
             (let ((src (cdr (assoc name v :test #'string=))))
               (when (and src target (= (length src) (length target))) (replace target src)))))
      (put "y-mech" (worm.solver:integrator-y-mech it))
      (put "y-neural" (worm.solver:integrator-y-neural it))
      (put "z" (worm.solver:integrator-z it))
      (put "aux" (worm.solver:integrator-aux it))
      (put "p" (worm.solver:integrator-params it)))
    (worm.solver:set-integrator-position it :time (getf c :time) :step (getf c :step) :h (getf c :h))
    (run-log run "состояние из ~A, шаг ~D" (namestring path) (getf c :step))
    (getf c :step)))

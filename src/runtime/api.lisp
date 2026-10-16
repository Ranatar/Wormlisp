;;;; src/runtime/api.lisp — API прогонов для REPL.
;;;;
;;;; Архитектура, «API для REPL». Функции принимают прогон последним
;;;; необязательным аргументом или ключом и по умолчанию берут *current-run*
;;;; — последний запущенный. Синхронные функции ждут ответа потока
;;;; симуляции; ошибки, случившиеся там, переподаются в вызывающем потоке.

(in-package #:worm.runtime)

(defvar *current-run* nil "Последний запущенный прогон.")
(defvar *runs* '() "Прогоны этого процесса, новые первыми.")

(defun the-run (run)
  (or run *current-run* (error 'run-error :message "прогона нет: сначала (start-run …)")))

(defun send (run op &rest args)
  "Отправить команду и вернуть обещание. У прогона без потока (пакетный
рабочий) команда выполняется сразу в вызывающем потоке, а :step считается
до конца здесь же; :resume без потока не имеет смысла."
  (let ((promise (make-promise)))
    (when (and (run-finished-p run) (not (eq op :call)))
      (error 'run-error :message (format nil "прогон ~A завершён (~(~A~))" (run-id run) (run-status run))))
    (if (run-thread run)
        (worm.platform:mailbox-send (run-mailbox run) (list* op promise args))
        (progn
          (when (eq op :resume)
            (error 'run-error :message "у прогона без потока нет непрерывного счёта: (step-run n)"))
          (handle-command run (list* op promise args))
          (let ((policy (or (run-policy-fn run) (setf (run-policy-fn run) (step-policy run)))))
            (loop while (run-active-p run) do (run-chunk run policy)))))
    promise))

(defun post-command (run op &rest args)
  "Отправить команду, не дожидаясь ответа (так команды шлёт сервер);
возвращает обещание."
  (apply #'send run op args))

(defun find-run (id)
  "Прогон процесса по идентификатору."
  (find id *runs* :key #'run-id :test #'string=))

(defun command (run op &rest args)
  "Отправить команду и дождаться ответа."
  (await (apply #'send run op args)))

;;; Сборка и запуск

(defun build-model (name &rest keys &key (params "water-v1") medium rings ring-nodes (cache t))
  "Собрать модель NAME (имя defmodel) в compiled-model. CACHE — брать
скомпилированный код из cache/codegen/ (повторная сборка — доли секунды)."
  (declare (ignore params medium rings ring-nodes))
  (apply #'worm.codegen:compile-model name :cache cache keys))

(defun seconds-to-steps (seconds h)
  (if (and seconds (plusp seconds)) (max 1 (round seconds h)) 0))

(defun start-run (model &key (params "water-v1") medium rings (dt 1d-4) record
                             (frames-every 0.01d0) (snapshot-every 0.01d0) (checkpoint-every 0)
                             (memory-checkpoint-every 0.05d0) (neural-every 10)
                             (policy :interactive) id directory paused (thread t))
  "Запустить прогон: MODEL — compiled-model или имя модели (тогда она
собирается с PARAMS и MEDIUM). Интервалы записи, снимков и контрольных
точек — в секундах модели; CHECKPOINT-EVERY — точки на диск (0 — только по
команде), MEMORY-CHECKPOINT-EVERY — кольцо в памяти для rollback. POLICY —
:interactive (пауза и решение человека), :batch или функция от условия.
THREAD NIL — без потока симуляции (пакетный рабочий): команды выполняются
в вызывающем потоке, прогон стоит на паузе между ними."
  (let* ((set (if (worm.params:param-set-p params) params (worm.params:load-param-set params)))
         (model (if (compiled-model-p model)
                    model
                    (worm.codegen:compile-model model :params set :medium medium :rings rings :cache t)))
         (h (float dt 0d0))
         (p (worm.codegen:model-param-vector model set))
         (it (multiple-value-bind (y0 yn0 z0) (worm.codegen:model-initial-state model)
               (worm.solver:make-integrator model :params p :y-mech y0 :y-neural yn0 :z z0 :h h
                                                  :neural-every neural-every
                                                  ;; Q кратен R (решатель, «Контрольные точки»)
                                                  :checkpoint-every (* neural-every
                                                                       (max 1 (round (seconds-to-steps memory-checkpoint-every h)
                                                                                     neural-every))))))
         (id (or id (worm.io:make-run-id (compiled-model-name model))))
         (run (%make-run :id id :model model :integrator it :param-set set :policy policy
                         :frame-every (max 1 (seconds-to-steps frames-every h))
                         :snapshot-every (max 1 (seconds-to-steps snapshot-every h))
                         :checkpoint-every (seconds-to-steps checkpoint-every h)
                         :started (iso-utc))))
    (allocate-buffers run)
    (worm.solver:save-checkpoint it)
    (open-run-directory run (uiop:ensure-directory-pathname
                             (or directory (merge-pathnames (format nil "~A/" id) (worm.io:runs-directory)))))
    (run-log run "начало: модель ~(~A~), набор ~(~A~), среда ~(~A~), h = ~,2E с, политика ~(~A~)"
             (compiled-model-name model) (worm.params:param-set-name set) (worm.codegen:model-medium model) h
             (if (functionp policy) :custom policy))
    (when record
      (setf (run-record-p run) t)
      (open-writer run))
    (set-status run (if (or paused (not thread)) :paused :running))
    (publish-snapshot run)
    (when thread (start-simulation-thread run))
    (push run *runs*)
    (setf *current-run* run)))

(defun pause (&optional run) (command (the-run run) :pause))
(defun resume (&optional run) (command (the-run run) :resume))

(defun step-run (n &optional run (timeout nil))
  "Сделать N шагов и остановиться; ждёт окончания (TIMEOUT в секундах, NIL —
сколько потребуется). Возвращает номер шага."
  (await (send (the-run run) :step n) :timeout timeout))

(defun run-status-of (&optional run) (run-status (the-run run)))

(defun stop-run (&optional run)
  "Завершить прогон: закрыть файлы, записать итог в meta.json."
  (let ((run (the-run run)))
    (prog1 (if (run-finished-p run) (run-status run) (command run :stop))
      (when (run-thread run)
        (worm.platform:join-thread (run-thread run) :timeout 30)))))

(defun wait-run (&key run (status '(:paused :problem :done :failed)) (timeout 60))
  "Дождаться, пока прогон перейдёт в одно из состояний STATUS."
  (let ((run (the-run run))
        (deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop until (member (run-status run) (if (listp status) status (list status)))
          do (when (> (get-internal-real-time) deadline)
               (error 'run-timeout :message (format nil "прогон не перешёл в ~(~S~) за ~D с" status timeout)))
             (sleep 0.01))
    (run-status run)))

(defun call-in-run (fn &optional run)
  "Выполнить (FN run) в потоке симуляции на границе шага и вернуть результат."
  (command (the-run run) :call fn))

;;; Параметры

(defun spec-of (name)
  (worm.params:find-parameter name))

(defun display-unit (spec) (worm.params:parameter-spec-unit spec))

(defun decimal-double (value)
  "Число в double-float; single-float из REPL (0.05) — по его десятичной
записи, а не по двоичному значению (0.0500000007…)."
  (if (typep value 'single-float)
      (let ((text (let ((*read-default-float-format* 'single-float)) (prin1-to-string value))))
        (let ((*read-default-float-format* 'double-float) (*read-eval* nil))
          (float (read-from-string text) 0d0)))
      (float value 0d0)))

(defun to-internal-value (spec value unit)
  "Значение VALUE в единицах UNIT (по умолчанию — единица реестра) во внутренних."
  (let ((unit (or unit (display-unit spec))))
    (unless (equal (worm.params:unit-dimension unit) (worm.params:parameter-spec-dimension spec))
      (error 'worm.params:unit-mismatch
             :message (format nil "~(~A~): единица ~A другой размерности, чем ~A"
                              (worm.params:parameter-spec-name spec) unit (display-unit spec))))
    (if unit (worm.params:to-internal (decimal-double value) unit) (decimal-double value))))

(defun param-index (model name)
  (position name (worm.codegen:model-param-names model)))

(defun check-live (run spec)
  "Индекс параметра в векторе p, если он допускает живую правку; иначе
ошибка с объяснением."
  (let* ((name (worm.params:parameter-spec-name spec))
         (index (param-index (run-model run) name)))
    (cond ((not (worm.params:parameter-spec-live-p spec))
           (error 'run-error
                  :message (format nil "~(~A~) не правится на ходу: от него зависят топология или величины, посчитанные при сборке (~A). Нужна пересборка: (rebuild run '~(~A~) :params <набор>)"
                                   name (worm.params:parameter-spec-doc spec) (compiled-model-name (run-model run)))))
          ((null index)
           (error 'run-error
                  :message (format nil "~(~A~) ядра модели ~(~A~) не читают напрямую: из него при сборке строятся поэлементные массивы или его модель не использует. Нужна пересборка: (rebuild run '~(~A~) :params <набор>)"
                                   name (compiled-model-name (run-model run)) (compiled-model-name (run-model run)))))
          (t index))))

(defun check-bounds (spec key value unit internal)
  (let ((bounds (worm.params:parameter-spec-bounds spec)))
    (when (and bounds (not (<= (first bounds) internal (second bounds))))
      (error 'worm.params:out-of-bounds
                     :message (let ((u (display-unit spec)))
                                (format nil "~(~A~) = ~G~@[ ~(~A~)~] вне границ [~G, ~G]~@[ ~(~A~)~]" key value (or unit u)
                                        (if u (worm.params:from-internal (first bounds) u) (first bounds))
                                        (if u (worm.params:from-internal (second bounds) u) (second bounds))
                                        u))))))

(defun prepare-param-change (run name value &key unit)
  "Проверить правку и вернуть (values имя-ключ индекс внутреннее-значение);
ошибки — как у SET-PARAM, но без перезапусков."
  (let* ((spec (spec-of name))
         (key (worm.params:parameter-spec-name spec))
         (index (check-live run spec))
         (internal (if (realp value)
                       (to-internal-value spec value unit)
                       (error 'run-error :message (format nil "~(~A~): значение ~S — не число" key value)))))
    (check-bounds spec key value unit internal)
    (values key index internal)))

(defun set-param (name value &key unit run (source :repl))
  "Изменить параметр на ходу. VALUE — в единицах реестра (или UNIT).
Параметр без живой правки отвергается с объяснением; значение вне границ —
условие out-of-bounds с перезапусками use-value и keep-old-value."
  (let* ((run (the-run run))
         (spec (spec-of name))
         (key (worm.params:parameter-spec-name spec))
         (index (check-live run spec))
         (internal (to-internal-value spec value unit)))
    (restart-case
        (progn
          (check-bounds spec key value unit internal)
          (second (command run :set-param key index internal source)))
      (use-value (v)
        :report "Задать другое значение (в единицах реестра)."
        :interactive (lambda () (format *query-io* "Значение: ") (list (read *query-io*)))
        (set-param name v :run run))
      (worm.params:keep-old-value ()
        :report "Оставить прежнее значение."
        (aref (worm.solver:integrator-params (run-integrator run)) index)))))

(defun param (name &optional run)
  "Текущее значение параметра в единицах реестра; второе значение — единица."
  (let* ((run (the-run run))
         (spec (spec-of name))
         (key (worm.params:parameter-spec-name spec))
         (index (param-index (run-model run) key))
         (internal (if index
                       (aref (worm.solver:integrator-params (run-integrator run)) index)
                       (worm.params:param-value (run-param-set run) key)))
         (unit (display-unit spec)))
    (values (if (and unit (realp internal)) (worm.params:from-internal internal unit) internal) unit)))

(defun load-params (designator &optional run)
  "Загрузить набор параметров: живые применяются сразу, остальные
перечисляются как требующие пересборки. Возвращает plist
(:applied ((имя старое новое) …) :needs-rebuild (имя …))."
  (let* ((run (the-run run))
         (set (if (worm.params:param-set-p designator) designator (worm.params:load-param-set designator)))
         (old (run-param-set run))
         (model (run-model run))
         (p (worm.solver:integrator-params (run-integrator run)))
         (changes '()) (rebuild '()))
    (loop for name across (worm.codegen:model-param-names model)
          for index from 0
          for value = (float (worm.params:param-value set name) 0d0)
          unless (= value (aref p index))
            do (if (worm.params:parameter-spec-live-p (spec-of name))
                   (push (list name index value) changes)
                   (push name rebuild)))
    (dolist (name (worm.params:param-names set))
      (unless (or (param-index model name) (member name rebuild))
        (let ((a (worm.params:param-entry-of old name)) (b (worm.params:param-entry-of set name)))
          (unless (and a (equalp (worm.params:param-entry-value a) (worm.params:param-entry-value b)))
            (push name rebuild)))))
    (let ((applied (command run :apply-params (nreverse changes) (and (null rebuild) set))))
      (when rebuild
        (run-log run "набор ~(~A~): требуют пересборки ~{~(~A~)~^, ~}" (worm.params:param-set-name set) rebuild))
      (list :applied (loop for (name old new) in applied collect (list name old new))
            :needs-rebuild (sort rebuild #'string< :key #'symbol-name)))))

;;; Состояние

(defun snapshot (&optional run) (run-snapshot (the-run run)))

(defun state-ref (&key ring node axis muscle curvature run)
  "Значение из последнего снимка: (state-ref :ring 10 :node 3 :axis :x) —
координата узла, мм; (state-ref :muscle 5) — активация мышцы;
(state-ref :ring 10 :curvature t) — кривизна в кольце, 1/мм; без ключей —
сам снимок."
  (let* ((s (snapshot run)))
    (cond (muscle (aref (snapshot-activations s) muscle))
          ((and ring curvature) (aref (snapshot-curvature s) ring))
          ((and ring node)
           (let ((base (* 3 (+ (* ring (snapshot-ring-nodes s)) node))))
             (if axis
                 (aref (snapshot-coords s) (+ base (ecase axis (:x 0) (:y 1) (:z 2))))
                 (subseq (snapshot-coords s) base (+ base 3)))))
          (t s))))

(defun save-checkpoint (&optional run)
  "Контрольная точка на диск, в checkpoints/ каталога прогона; путь."
  (command (the-run run) :checkpoint))

(defun rollback (&rest args)
  "Откатиться: (rollback [TARGET [RUN]] [:params P] [:run RUN]). TARGET —
номер точки в памяти (0 — последняя) или путь к точке на диске. Параметры
по умолчанию не откатываются — правка, сделанная после снимка, остаётся;
:params t восстанавливает и их. Возвращает номер шага."
  (let ((target 0) (run nil))
    (unless (or (null args) (keywordp (first args)))
      (setf target (pop args))
      (unless (or (null args) (keywordp (first args)))
        (setf run (pop args))))
    (destructuring-bind (&key params ((:run key-run) nil)) args
      (command (the-run (or key-run run)) :rollback target (and params t)))))

(defun record (on &optional run)
  "Включить или выключить запись кадров."
  (command (the-run run) :record (and on t)))

(defun record-to (directory &optional run)
  "Продолжить запись в новый каталог прогона."
  (command (the-run run) :record-to directory))

;;; Решения о сбоях

(defun problem (&optional run (stream *standard-output*))
  "Сбой, ждущий решения: печатает условие, шаг, элемент и варианты."
  (let* ((run (the-run run))
         (p (run-problem run)))
    (when stream
      (if p
          (format stream "~&Прогон ~A ждёт решения.~%  ~A~%  шаг ~D, t = ~,5F с, h = ~,2E с~@[, элемент ~A~]~%  варианты:~%~{    (choose '~(~A~))  — ~A~%~}"
                  (run-id run) (problem-report p) (problem-step p) (problem-time p) (problem-h p) (problem-where p)
                  (loop for (name . text) in (problem-restarts p) collect name collect text))
          (format stream "~&Прогон ~A: сбоя нет, состояние ~(~A~).~%" (run-id run) (run-status run))))
    p))

(defun choose (restart &optional run)
  "Выбрать перезапуск для сбоя, ждущего решения."
  (let ((run (the-run run)))
    (unless (run-problem run)
      (error 'run-error :message (format nil "прогон ~A не ждёт решения" (run-id run))))
    (command run :choose restart)))

(defun debug-problem (&optional run)
  "Открыть отладчик в потоке симуляции на условии сбоя. Терминал ему
передаёт (sb-thread:release-foreground) в REPL."
  (let ((run (the-run run)))
    (unless (run-problem run)
      (error 'run-error :message (format nil "прогон ~A не ждёт решения" (run-id run))))
    (command run :debug *terminal-io*)))

;;; Пересборка

(defun transfer-blocks (old-layout new-layout old new)
  "Перенести совпадающие по (экземпляр переменная) блоки вектора."
  (let ((copied '()))
    (loop for (inst var off count) in new-layout
          for src = (find-if (lambda (b) (and (eq (first b) inst) (eq (second b) var))) old-layout)
          when (and src (= (fourth src) count))
            do (replace new old :start1 off :start2 (third src) :end2 (+ (third src) count))
               (push (list inst var) copied))
    (nreverse copied)))

(defun transfer-neural (old-model new-model old-yn new-yn)
  "Перенести совпадающие по (экземпляр переменная) блоки y-neural."
  (transfer-blocks (worm.codegen:model-neural-states old-model) (worm.codegen:model-neural-states new-model)
                   old-yn new-yn))

(defun same-topology-p (a b)
  (let ((ta (compiled-model-topology a)) (tb (compiled-model-topology b)))
    (and (= (worm.topology:tube-rings ta) (worm.topology:tube-rings tb))
         (= (worm.topology:tube-ring-nodes ta) (worm.topology:tube-ring-nodes tb))
         (= (compiled-model-n-mech a) (compiled-model-n-mech b)))))

(defun replace-model (run new-model set keep-params)
  "Выполняется в потоке симуляции: новая модель, перенос состояния."
  (let* ((old-model (run-model run))
         (old (run-integrator run))
         (same (same-topology-p old-model new-model))
         (p (worm.codegen:model-param-vector new-model set))
         (kept-params '()))
    (when keep-params
      (loop for name across (worm.codegen:model-param-names new-model)
            for index from 0
            for old-index = (param-index old-model name)
            when old-index
              do (setf (aref p index) (aref (worm.solver:integrator-params old) old-index))
                 (push name kept-params)))
    (multiple-value-bind (y0 yn0 z0) (worm.codegen:model-initial-state new-model)
      (when same (replace y0 (worm.solver:integrator-y-mech old)))
      (let* ((neural (append (transfer-neural old-model new-model (worm.solver:integrator-y-neural old) yn0)
                             (transfer-blocks (worm.codegen:model-discrete-states old-model)
                                              (worm.codegen:model-discrete-states new-model)
                                              (worm.solver:integrator-z old) z0)))
             (it (worm.solver:make-integrator new-model :params p :y-mech y0 :y-neural yn0 :z z0
                                                        :aux (and same (= (compiled-model-n-aux new-model)
                                                                          (length (worm.solver:integrator-aux old)))
                                                                  (worm.solver:integrator-aux old))
                                                        :h (worm.solver:integrator-h old)
                                                        :time (worm.solver:integrator-time old)
                                                        :step (worm.solver:integrator-step old)
                                                        :neural-every (worm.solver:integrator-neural-every old)
                                                        :checkpoint-every (worm.solver::integrator-checkpoint-every old)))
             (frames-compatible (and same (= (model-muscle-count old-model) (model-muscle-count new-model)))))
        (unless frames-compatible
          (close-writer run)
          (setf (run-record-p run) nil))
        (setf (run-model run) new-model (run-integrator run) it (run-param-set run) set)
        (allocate-buffers run)
        (worm.solver:save-checkpoint it)
        (write-run-config run)
        (write-run-meta run)
        (run-log run "пересборка: модель ~(~A~), тело ~:[в начальном положении~;перенесено~], нервно-мышечное состояние: ~{~(~{~A.~A~}~)~^, ~}~:[; запись остановлена — другой формат кадра~;~]"
                 (compiled-model-name new-model) same neural frames-compatible)
        (publish-snapshot run)
        (list :body (if same :kept :reset) :neural neural :params-kept (nreverse kept-params)
              :recording (run-record-p run))))))

(defun rebuild (model-name &key params medium run (keep-params (null params)))
  "Пересобрать модель и перенести совместимое состояние: тело — если
топология та же, нервно-мышечные переменные — по именам. Параметры — из
набора PARAMS; без него — прежние значения тех же имён."
  (let* ((run (the-run run))
         (set (cond ((null params) (run-param-set run))
                    ((worm.params:param-set-p params) params)
                    (t (worm.params:load-param-set params))))
         (model (worm.codegen:compile-model model-name :params set
                                                       :medium (or medium (worm.codegen:model-medium (run-model run)))
                                                       :rings (model-rings (run-model run))
                                                       :cache t))
         (was (run-status run)))
    (unless (eq was :paused) (pause run))
    (prog1 (command run :replace model set keep-params)
      (when (member was '(:running :stepping)) (resume run)))))

;;; Описание прогона для клиентов

(defun shown-value (spec internal)
  (let ((u (and spec (display-unit spec))))
    (if (and u (realp internal)) (worm.params:from-internal internal u) internal)))

(defun run-parameters (&optional run)
  "Параметры прогона: список plist (:name :value :unit :min :max :live :doc).
Живые — те, что можно править на ходу (решение 25)."
  (let* ((run (the-run run))
         (model (run-model run))
         (p (worm.solver:integrator-params (run-integrator run)))
         (set (run-param-set run))
         (names (union (coerce (worm.codegen:model-param-names model) 'list) (worm.params:param-names set))))
    (loop for name in (sort (copy-list names) #'string< :key #'symbol-name)
          for spec = (worm.params:find-parameter name nil)
          for index = (param-index model name)
          for internal = (if index (aref p index)
                             (let ((e (worm.params:param-entry-of set name))) (and e (worm.params:param-entry-value e))))
          for bounds = (and spec (worm.params:parameter-spec-bounds spec))
          when (realp internal)
            collect (list :name name :value (shown-value spec internal)
                          :unit (and spec (display-unit spec))
                          :min (and bounds (shown-value spec (first bounds)))
                          :max (and bounds (shown-value spec (second bounds)))
                          :live (and spec index (worm.params:parameter-spec-live-p spec) t)
                          :doc (if spec (worm.params:parameter-spec-doc spec) "")))))

(defun run-description (&optional run)
  "Сведения о прогоне для клиентов: plist."
  (let* ((run (the-run run))
         (model (run-model run))
         (tube (compiled-model-topology model))
         (anatomy (compiled-model-anatomy model))
         (it (run-integrator run)))
    (list :id (run-id run) :model (compiled-model-name model) :medium (worm.codegen:model-medium model)
          :params (worm.params:param-set-name (run-param-set run))
          :rings (worm.topology:tube-rings tube) :ring-nodes (worm.topology:tube-ring-nodes tube)
          :length (worm.topology:tube-length tube) :radius-max (worm.topology:tube-radius-max tube)
          :h (worm.solver:integrator-h it)
          :muscles (if anatomy
                       (map 'list (lambda (m) (list :name (worm.anatomy:muscle-name m)
                                                    :quadrant (worm.anatomy:muscle-quadrant m)
                                                    :row (worm.anatomy:muscle-row m)
                                                    :from (worm.anatomy:muscle-from m)
                                                    :peak (worm.anatomy:muscle-peak m)
                                                    :to (worm.anatomy:muscle-to m)))
                            (worm.anatomy:anatomy-muscles anatomy))
                       '()))))

(defun run-param-change-log (&optional run)
  "Правки параметров: ((шаг имя старое новое источник) …), новые первыми;
значения — в единицах реестра."
  (loop for (step name old new source) in (run-param-changes (the-run run))
        for spec = (worm.params:find-parameter name nil)
        collect (list step name (shown-value spec old) (shown-value spec new) source)))

(defun abort-run-with (run reason)
  "Завершить прогон со статусом failed, записав причину в log.txt (пакетный
рабочий при ошибке вне шага)."
  (run-log run "прогон прерван: ~A" reason)
  (finish-run run :failed))

(defun list-runs ()
  "Прогоны процесса: (id модель состояние шаг)."
  (loop for r in *runs*
        collect (list (run-id r) (compiled-model-name (run-model r)) (run-status r)
                      (let ((s (run-snapshot r))) (and s (snapshot-step s))))))

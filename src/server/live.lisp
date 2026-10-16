;;;; src/server/live.lisp — WebSocket /live: кадры и управляющие сообщения.
;;;;
;;;; Архитектура, «Отправка кадров»: у каждого соединения свой поток
;;;; отправки. Раз в 1/частоту он берёт последний снимок прогона и, если шаг
;;;; новее отправленного, кодирует и отправляет кадр. Очереди нет: медленный
;;;; клиент пропускает промежуточные снимки, а если он совсем не читает,
;;;; ждёт только его поток отправки. Поток симуляции о клиентах не знает:
;;;; сервер читает снимки и шлёт команды без ожидания ответа.
;;;;
;;;; Управляющие сообщения — JSON-объекты с полем "type". Любое искажённое
;;;; или неизвестное сообщение получает ответ {"type": "error"} и не доходит
;;;; до прогона.

(in-package #:worm.server)

(defclass live-resource (hunchensocket:websocket-resource) ()
  (:default-initargs :client-class 'live-client))

(defclass live-client (hunchensocket:websocket-client)
  ((run :initform nil :accessor client-run)
   (pinned :initform nil :accessor client-pinned)        ; прогон выбран явно (?run= или select)
   (rate :initform 20d0 :accessor client-rate)           ; кадров в секунду
   (alive :initform t :accessor client-alive)
   (sent-step :initform -1 :accessor client-sent-step)
   (sent-status :initform nil :accessor client-sent-status)
   (sent-problem :initform nil :accessor client-sent-problem)
   (sent-changes :initform 0 :accessor client-sent-changes)
   (frame-buffer :initform nil :accessor client-frame-buffer)
   (thread :initform nil :accessor client-thread)))

(defvar *live* (make-instance 'live-resource))

(defun live-dispatch (request)
  (when (string= (hunchentoot:script-name request) "/live") *live*))

(pushnew 'live-dispatch hunchensocket:*websocket-dispatch-table*)

;;; Отправка

(defun send-json (client object)
  (hunchensocket:send-text-message client (worm.io:json-string object)))

(defun send-error (client kind control &rest args)
  (send-json client (worm.io:json-object "type" "error" "kind" kind "message" (apply #'format nil control args))))

(defun lower (x) (if (symbolp x) (string-downcase (symbol-name x)) x))

(defun json-number (x) (if (and (realp x) (not (and (floatp x) (or (sb-ext:float-nan-p x) (sb-ext:float-infinity-p x))))) x 'null))

(defun hello (client)
  (let ((run (client-run client)))
    (send-json
     client
     (if (null run)
         (worm.io:json-object "type" "hello" "run" 'null "readOnly" (and *read-only* t))
         (let ((d (worm.runtime:run-description run)))
           (worm.io:json-object
            "type" "hello" "readOnly" (and *read-only* t) "rate" (client-rate client)
            "run" (worm.io:json-object
                   "id" (getf d :id) "model" (lower (getf d :model)) "medium" (lower (getf d :medium))
                   "params" (lower (getf d :params)) "h" (getf d :h)
                   "topology" (worm.io:json-object "rings" (getf d :rings) "ringNodes" (getf d :ring-nodes)
                                                   "length" (getf d :length) "radiusMax" (getf d :radius-max))
                   "muscles" (map 'vector (lambda (m)
                                            (worm.io:json-object "name" (lower (getf m :name))
                                                                 "quadrant" (lower (getf m :quadrant))
                                                                 "row" (lower (getf m :row))
                                                                 "from" (getf m :from) "peak" (getf m :peak) "to" (getf m :to)))
                                  (getf d :muscles)))
            "params" (map 'vector (lambda (p)
                                    (worm.io:json-object "name" (lower (getf p :name))
                                                         "value" (json-number (getf p :value))
                                                         "unit" (if (getf p :unit) (lower (getf p :unit)) 'null)
                                                         "min" (json-number (getf p :min)) "max" (json-number (getf p :max))
                                                         "live" (and (getf p :live) t)
                                                         "doc" (getf p :doc)))
                          (worm.runtime:run-parameters run))))))))

(defun send-status (client snapshot)
  (let ((run (client-run client)))
    (send-json client (worm.io:json-object
                       "type" "status" "run" (worm.runtime:run-id run)
                       "status" (lower (worm.runtime:run-status run))
                       "step" (if snapshot (worm.runtime:snapshot-step snapshot) 0)
                       "time" (if snapshot (worm.runtime:snapshot-time snapshot) 0)
                       "h" (if snapshot (worm.runtime:snapshot-h snapshot) 0)))))

(defun send-problem (client problem)
  (send-json client (worm.io:json-object
                     "type" "error" "kind" "problem"
                     "cause" (lower (worm.runtime:problem-cause problem))
                     "step" (worm.runtime:problem-step problem) "time" (worm.runtime:problem-time problem)
                     "h" (worm.runtime:problem-h problem)
                     "where" (or (worm.runtime:problem-where problem) 'null)
                     "message" (worm.runtime:problem-report problem)
                     "restarts" (map 'vector (lambda (r) (worm.io:json-object "name" (lower (car r)) "report" (cdr r)))
                                     (worm.runtime:problem-restarts problem)))))

(defun send-frame (client snapshot)
  (let* ((coords (worm.runtime:snapshot-coords snapshot))
         (acts (worm.runtime:snapshot-activations snapshot))
         (size (worm.io:frame-size (floor (length coords) 3) (length acts)))
         (buf (let ((b (client-frame-buffer client)))
                (if (and b (= (length b) size)) b
                    (setf (client-frame-buffer client) (make-array size :element-type '(unsigned-byte 8)))))))
    (worm.io:encode-frame buf 0 (worm.runtime:snapshot-step snapshot) (worm.runtime:snapshot-time snapshot) coords acts)
    (hunchensocket:send-binary-message client buf)))

(defun send-param-changes (client)
  (let* ((log (worm.runtime:run-param-change-log (client-run client)))
         (fresh (- (length log) (client-sent-changes client))))
    (when (plusp fresh)
      (dolist (c (reverse (subseq log 0 fresh)))
        (destructuring-bind (step name old new source) c
          (declare (ignore old))
          (send-json client (worm.io:json-object "type" "param-changed" "name" (lower name)
                                                 "value" (json-number new) "step" step "source" (lower source)))))
      (setf (client-sent-changes client) (length log)))))

(defun follow-current-run (client)
  "Без явного выбора клиент смотрит последний запущенный прогон."
  (unless (client-pinned client)
    (let ((current worm.runtime:*current-run*))
      (unless (eq current (client-run client))
        (setf (client-run client) current
              (client-sent-step client) -1 (client-sent-status client) nil
              (client-sent-problem client) nil
              (client-sent-changes client) (if current (length (worm.runtime:run-param-change-log current)) 0))
        (hello client)))))

(defun sender-loop (client)
  (let ((last-status-time 0) (last-ping (get-internal-real-time)))
    (loop while (client-alive client)
          do (sleep (/ 1d0 (client-rate client)))
             (follow-current-run client)
             (let ((run (client-run client))
                   (now (get-internal-real-time)))
               (when run
                 (let ((snapshot (worm.runtime:snapshot run))
                       (problem (worm.runtime:run-problem run)))
                   (when (and snapshot (> (worm.runtime:snapshot-step snapshot) (client-sent-step client)))
                     (send-frame client snapshot)
                     (setf (client-sent-step client) (worm.runtime:snapshot-step snapshot)))
                   (unless (eq problem (client-sent-problem client))
                     (when problem (send-problem client problem))
                     (setf (client-sent-problem client) problem))
                   (when (or (not (eq (worm.runtime:run-status run) (client-sent-status client)))
                             (> (- now last-status-time) internal-time-units-per-second))
                     (send-status client snapshot)
                     (setf (client-sent-status client) (worm.runtime:run-status run) last-status-time now))
                   (send-param-changes client)))
               (when (> (- now last-ping) (* 30 internal-time-units-per-second))
                 (hunchensocket:send-ping client)
                 (setf last-ping now))))))

(defmethod hunchensocket:client-connected ((resource live-resource) (client live-client))
  (let* ((id (hunchentoot:get-parameter "run" (hunchensocket:client-request client)))
         (run (and id (worm.runtime:find-run id))))
    (when run (setf (client-run client) run (client-pinned client) t))
    (if run
        (hello client)
        (follow-current-run client))
    (when (and id (null run))
      (send-error client "protocol" "прогона ~A в этом процессе нет; показан текущий" id))
    (let ((acceptor hunchentoot:*acceptor*))
      (setf (client-thread client)
            (worm.platform:make-thread
             (lambda ()
               (let ((hunchentoot:*acceptor* acceptor))
                 (handler-case (sender-loop client)
                   (error (e)
                     (setf (client-alive client) nil)
                     ;; обрыв соединения — обычное дело, остальное — в журнал сервера
                     (ignore-errors
                      (hunchentoot:log-message* (if (typep e 'stream-error) :info :warning)
                                                "live: поток отправки остановлен: ~A" e))))))
             :name "worm live sender")))))

(defmethod hunchensocket:client-disconnected ((resource live-resource) (client live-client))
  (setf (client-alive client) nil))

;;; Приём

(defun keyword-named (string)
  "Существующее ключевое слово по имени из сети (новые символы не создаются)."
  (and (stringp string) (find-symbol (string-upcase string) :keyword)))

(defun field (message name) (gethash name message))

(defparameter +control-messages+ '("pause" "resume" "step" "set-param" "choose"))

(defun handle-message (client message)
  (let ((type (and (hash-table-p message) (field message "type"))))
    (cond
      ((not (stringp type))
       (send-error client "protocol" "сообщение — объект JSON с полем \"type\""))
      ((and *read-only* (member type +control-messages+ :test #'string=))
       (send-error client "read-only" "сервер запущен с :read-only t: управление из браузера запрещено"))
      ((string= type "rate")
       (let ((fps (field message "fps")))
         (if (and (realp fps) (<= 1 fps 60))
             (setf (client-rate client) (float fps 0d0))
             (send-error client "protocol" "rate: fps — число от 1 до 60"))))
      ((string= type "select")
       (let ((run (and (stringp (field message "run")) (worm.runtime:find-run (field message "run")))))
         (if run
             (progn (setf (client-run client) run (client-pinned client) t
                          (client-sent-step client) -1 (client-sent-status client) nil (client-sent-problem client) nil
                          (client-sent-changes client) (length (worm.runtime:run-param-change-log run)))
                    (hello client))
             (send-error client "protocol" "select: прогона ~S нет" (field message "run")))))
      ((null (client-run client))
       (send-error client "rejected" "прогона нет: запустите его в REPL (start-run …)"))
      ((string= type "pause") (worm.runtime:post-command (client-run client) :pause))
      ((string= type "resume") (worm.runtime:post-command (client-run client) :resume))
      ((string= type "step")
       (let ((n (field message "n")))
         (if (and (integerp n) (<= 1 n 10000000))
             (worm.runtime:post-command (client-run client) :step n)
             (send-error client "protocol" "step: n — целое от 1 до 10⁷"))))
      ((string= type "set-param")
       (let ((name (keyword-named (field message "name"))) (value (field message "value")))
         (if (null name)
             (send-error client "rejected" "параметра ~S нет" (field message "name"))
             (handler-case
                 (multiple-value-bind (key index internal)
                     (worm.runtime:prepare-param-change (client-run client) name value)
                   (worm.runtime:post-command (client-run client) :set-param key index internal :browser))
               (worm.core:worm-error (e) (send-error client "rejected" "~A" e))))))
      ((string= type "choose")
       (let* ((problem (worm.runtime:run-problem (client-run client)))
              (name (field message "restart"))
              (restart (and problem (stringp name)
                            (find name (worm.runtime:problem-restarts problem)
                                  :key (lambda (r) (symbol-name (car r))) :test #'string-equal))))
         (cond ((null problem) (send-error client "rejected" "сбоя, ждущего решения, нет"))
               ((null restart) (send-error client "rejected" "перезапуска ~S нет" name))
               (t (worm.runtime:post-command (client-run client) :choose (car restart))))))
      (t (send-error client "protocol" "неизвестное сообщение ~S" type)))))

(defmethod hunchensocket:text-message-received ((resource live-resource) (client live-client) text)
  (handler-case
      (let ((message (handler-case (worm.io:parse-json text)
                       (error () (send-error client "protocol" "не JSON") (return-from hunchensocket:text-message-received)))))
        (handle-message client message))
    (error (e)
      (hunchentoot:log-message* :warning "live: сообщение ~S: ~A" (subseq text 0 (min 200 (length text))) e)
      (ignore-errors (send-error client "protocol" "сообщение не обработано: ~A" e)))))

(defmethod hunchensocket:check-message ((resource live-resource) (client live-client)
                                        (opcode (eql hunchensocket::+binary-frame+)) length total)
  ;; двоичное сообщение от клиента не закрывает соединение, а получает error
  (when (> total 65536) (hunchensocket::websocket-error 1009 "сообщение слишком велико")))

(defmethod hunchensocket:binary-message-received ((resource live-resource) (client live-client) binary)
  (declare (ignore binary))
  (ignore-errors (send-error client "protocol" "двоичные сообщения сервер не принимает")))

(defun live-clients () (hunchensocket:clients *live*))

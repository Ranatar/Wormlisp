;;;; tests/live.lisp — WebSocket /live (этап 9).
;;;;
;;;; Условия готовности: время шага с подключённым клиентом не отличается от
;;;; времени без клиента сверх шума замера; медленный клиент пропускает
;;;; кадры, симуляция его не ждёт; set-param и choose из браузера работают,
;;;; :read-only их запрещает; искажённое сообщение клиента не влияет на
;;;; прогон. Клиент — минимальный WebSocket по RFC 6455 поверх usocket.

(in-package #:worm.tests)

(in-suite server)

;;; Клиент

(defstruct ws socket stream)

(defun crlf-line (stream)
  (let ((bytes '()))
    (loop for b = (read-byte stream)
          until (= b 10)
          unless (= b 13) do (push b bytes))
    (sb-ext:octets-to-string (coerce (nreverse bytes) '(vector (unsigned-byte 8))) :external-format :utf-8)))

(defun ws-connect (port &optional (path "/live"))
  (let* ((socket (usocket:socket-connect "127.0.0.1" port :element-type '(unsigned-byte 8)))
         (stream (usocket:socket-stream socket))
         (key (cl-base64:usb8-array-to-base64-string
               (coerce (loop repeat 16 collect (random 256)) '(vector (unsigned-byte 8))))))
    (write-sequence (sb-ext:string-to-octets
                     (format nil "GET ~A HTTP/1.1~C~CHost: localhost~C~CUpgrade: websocket~C~CConnection: Upgrade~C~CSec-WebSocket-Key: ~A~C~CSec-WebSocket-Version: 13~C~C~C~C"
                             path #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed
                             key #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed)
                     :external-format :latin-1)
                    stream)
    (force-output stream)
    (let ((status (crlf-line stream)))
      (loop for line = (crlf-line stream) until (string= line ""))
      (unless (search "101" status)
        (error "рукопожатие: ~A" status)))
    (make-ws :socket socket :stream stream)))

(defun ws-close (ws) (ignore-errors (usocket:socket-close (ws-socket ws))))

(defun ws-send (ws payload &key (opcode 1))
  "Кадр клиента: с маской, как требует RFC 6455."
  (let* ((data (if (stringp payload) (sb-ext:string-to-octets payload :external-format :utf-8) payload))
         (n (length data))
         (mask (coerce (loop repeat 4 collect (random 256)) '(vector (unsigned-byte 8))))
         (out (ws-stream ws)))
    (write-byte (logior #x80 opcode) out)
    (cond ((< n 126) (write-byte (logior #x80 n) out))
          ((< n 65536) (write-byte (logior #x80 126) out) (write-byte (ash n -8) out) (write-byte (logand n 255) out))
          (t (write-byte (logior #x80 127) out) (loop for k from 7 downto 0 do (write-byte (ldb (byte 8 (* 8 k)) n) out))))
    (write-sequence mask out)
    (dotimes (i n) (write-byte (logxor (aref data i) (aref mask (mod i 4))) out))
    (force-output out)))

(defun ws-send-json (ws &rest pairs)
  (ws-send ws (worm.io:json-string (apply #'worm.io:json-object pairs))))

(defun ws-read (ws &key (timeout 10))
  "Следующее сообщение: (values опкод данные) или NIL по таймауту."
  (when (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
          ;; данные могут уже лежать в буфере потока; ожидание сокета
          ;; прерывается сигналами (сборка мусора) — тогда ждём дальше
          (loop (when (listen (ws-stream ws)) (return t))
                (let ((left (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)))
                  (when (<= left 0) (return nil))
                  (when (usocket:wait-for-input (ws-socket ws) :timeout (min left 1) :ready-only t)
                    (return t)))))
    (let* ((in (ws-stream ws))
           (b0 (read-byte in)) (b1 (read-byte in))
           (len (logand b1 127)))
      (setf len (case len
                  (126 (logior (ash (read-byte in) 8) (read-byte in)))
                  (127 (let ((v 0)) (dotimes (k 8 v) (setf v (logior (ash v 8) (read-byte in))))))
                  (t len)))
      (let ((data (make-array len :element-type '(unsigned-byte 8))))
        (read-sequence data in)
        (values (logand b0 15) data)))))

(defun ws-next (ws predicate &key (timeout 15))
  "Первое сообщение, для которого (PREDICATE опкод значение) истинно;
значение — хэш JSON для текста и кадр для двоичных данных."
  (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop
      (let ((left (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)))
        (when (<= left 0) (return nil))
        (multiple-value-bind (opcode data) (ws-read ws :timeout left)
          (unless opcode (return nil))
          (let ((value (case opcode
                         (1 (worm.io:parse-json (sb-ext:octets-to-string data :external-format :utf-8)))
                         (2 (worm.io:decode-frame data 0))
                         (t data))))
            (when (funcall predicate opcode value) (return value))))))))

(defun of-type (type)
  (lambda (opcode value) (and (= opcode 1) (equal type (gethash "type" value)))))

(defun binary-frame (opcode value) (declare (ignore value)) (= opcode 2))

(defmacro with-live ((ws port) &body body)
  `(let ((,ws (ws-connect ,port)))
     (unwind-protect (progn ,@body) (ws-close ,ws))))

;;; Тесты

(test live-sends-hello-status-and-frames
  (with-test-run (run "live" :snapshot-every 0.005d0)
    (worm.runtime:resume run)
    (with-test-server (port)
      (with-live (ws port)
        (let ((hello (ws-next ws (of-type "hello"))))
          (is (not (null hello)))
          (when hello
            (let ((r (gethash "run" hello)))
              (is (equal (worm.runtime:run-id run) (gethash "id" r)))
              (is (= 8 (gethash "rings" (gethash "topology" r))))
              (is (= 95 (length (gethash "muscles" r)))))
            (let* ((params (gethash "params" hello))
                   (find (lambda (n) (find n params :key (lambda (h) (gethash "name" h)) :test #'equal))))
              (is (eq t (gethash "live" (funcall find "tau-ca"))))
              (is (equal "ms" (gethash "unit" (funcall find "tau-ca"))))
              (is (null (gethash "live" (funcall find "prestress"))) "p₀ без живой правки"))))
        (is (not (null (ws-next ws (of-type "status")))))
        (let ((a (ws-next ws #'binary-frame)) (b (ws-next ws #'binary-frame)))
          (is (and a b (> (worm.io:frame-step b) (worm.io:frame-step a))) "кадры идут")
          (is (and a (= (* 3 8 8) (length (worm.io:frame-coords a))))))))))

(test browser-set-param-and-read-only
  (with-test-run (run "live-param")
    (with-test-server (port)
      (with-live (ws port)
        (ws-next ws (of-type "hello"))
        (ws-send-json ws "type" "set-param" "name" "tau-ca" "value" 70)
        (let ((changed (ws-next ws (of-type "param-changed"))))
          (is (and changed (equal "tau-ca" (gethash "name" changed))))
          (is (and changed (equal "browser" (gethash "source" changed)))))
        (is (worm.core:approx= 70d0 (worm.runtime:param :tau-ca run) :rel 1d-9))
        (ws-send-json ws "type" "set-param" "name" "prestress" "value" 10)
        (let ((e (ws-next ws (of-type "error"))))
          (is (and e (equal "rejected" (gethash "kind" e))))
          (is (and e (search "rebuild" (gethash "message" e)))))
        (ws-send-json ws "type" "set-param" "name" "tau-ca" "value" -3)
        (is (equal "rejected" (gethash "kind" (ws-next ws (of-type "error")))))
        (ws-send-json ws "type" "step" "n" 25)
        (worm.runtime:wait-run :run run :status :paused)
        (sleep 0.2)
        (is (= 25 (worm.runtime:snapshot-step (worm.runtime:snapshot run))))))
    (with-test-server (port :read-only t)
      (with-live (ws port)
        (is (eq t (gethash "readOnly" (ws-next ws (of-type "hello")))))
        (dolist (msg '(("type" "set-param" "name" "tau-ca" "value" 50) ("type" "resume") ("type" "step" "n" 5)
                       ("type" "choose" "restart" "abort-run")))
          (apply #'ws-send-json ws msg)
          (is (equal "read-only" (gethash "kind" (ws-next ws (of-type "error"))))))
        (ws-send-json ws "type" "rate" "fps" 10)
        (sleep 0.3)
        (is (worm.core:approx= 70d0 (worm.runtime:param :tau-ca run) :rel 1d-9) "правка не прошла")
        (is (eq :paused (worm.runtime:run-status run)))
        (is (= 25 (worm.runtime:snapshot-step (worm.runtime:snapshot run))))))))

(test browser-chooses-a-restart
  (with-test-run (run "live-choose")
    (worm.runtime:step-run 50 run)
    (with-test-server (port)
      (with-live (ws port)
        (ws-next ws (of-type "hello"))
        (collapse-a-compartment run)
        (ws-send-json ws "type" "resume")
        (let ((e (ws-next ws (lambda (op v) (and (= op 1) (equal "error" (gethash "type" v))
                                                 (equal "problem" (gethash "kind" v)))))))
          (is (not (null e)) "сбой пришёл в браузер")
          (when e
            (is (equal "volume-collapse" (gethash "cause" e)))
            (is (find "rollback-to-checkpoint" (gethash "restarts" e) :key (lambda (h) (gethash "name" h)) :test #'equal))))
        (ws-send-json ws "type" "choose" "restart" "no-such")
        (is (equal "rejected" (gethash "kind" (ws-next ws (of-type "error")))))
        (ws-send-json ws "type" "choose" "restart" "rollback-to-checkpoint")
        (is (ws-next ws (lambda (op v) (and (= op 1) (equal "status" (gethash "type" v))
                                            (equal "running" (gethash "status" v))))))
        (is (null (worm.runtime:run-problem run)))
        (worm.runtime:pause run)))))

(test malformed-messages-do-not-touch-the-run
  (with-test-run (run "live-garbage")
    (worm.runtime:step-run 40 run)
    (let ((before (run-state run)))
      (with-test-server (port)
        (with-live (ws port)
          (ws-next ws (of-type "hello"))
          (dolist (text '("{{{" "[1,2,3]" "42" "\"pause\"" "{\"type\": 5}" "{\"type\": \"explode\"}"
                          "{\"type\": \"step\", \"n\": \"many\"}" "{\"type\": \"step\", \"n\": -4}"
                          "{\"type\": \"set-param\", \"name\": \"no-such-param-xyz\", \"value\": 1}"
                          "{\"type\": \"set-param\", \"name\": \"tau-ca\", \"value\": \"fast\"}"
                          "{\"type\": \"rate\", \"fps\": 1e9}" "{\"type\": \"choose\", \"restart\": 7}"))
            (ws-send ws text)
            (is (not (null (ws-next ws (of-type "error")))) "ответ error на ~A" text))
          (ws-send ws (coerce #(1 2 3 4) '(vector (unsigned-byte 8))) :opcode 2)
          (is (not (null (ws-next ws (of-type "error")))) "двоичное сообщение")
          (is (equalp before (run-state run)) "состояние прогона не изменилось")
          (is (eq :paused (worm.runtime:run-status run)))
          (is (null (find-symbol "NO-SUCH-PARAM-XYZ" :keyword)) "имена из сети не создают символов")
          ;; соединение живо и управление работает
          (ws-send-json ws "type" "step" "n" 10)
          (worm.runtime:wait-run :run run :status :paused)
          (sleep 0.2)
          (is (= 50 (fourth (run-state run)))))))))

(test slow-client-skips-frames-and-is-not-waited-for
  (with-test-run (run "live-slow" :snapshot-every 0.001d0)
    (worm.runtime:resume run)
    (with-test-server (port)
      ;; совсем не читающий клиент: его поток отправки упрётся в буфер сокета
      (let ((stuck (ws-connect port)))
        (unwind-protect
             (with-live (ws port)
               (ws-send-json ws "type" "rate" "fps" 5)
               (let ((steps (loop repeat 6
                                  for f = (ws-next ws #'binary-frame)
                                  when f collect (worm.io:frame-step f))))
                 (is (= 6 (length steps)))
                 ;; снимок — каждый шаг, кадр — пять раз в секунду
                 (is (every (lambda (a b) (> (- b a) 20)) steps (rest steps))
                     "между кадрами пропущены снимки: ~S" steps))
               (let ((a (worm.runtime:snapshot-step (worm.runtime:snapshot run))))
                 (sleep 1)
                 (is (> (worm.runtime:snapshot-step (worm.runtime:snapshot run)) (+ a 100))
                     "счёт идёт, хотя один клиент не читает")))
          (ws-close stuck))))))

(defun timed-steps (run n)
  (let ((t0 (get-internal-real-time)))
    (worm.runtime:step-run n run)
    (/ (- (get-internal-real-time) t0) internal-time-units-per-second 1d0)))

(test a-connected-client-does-not-slow-the-step
  ;; Шаг меряется попеременно без клиента и с клиентом на 30 кадрах в
  ;; секунду; сравниваются медианы трёх замеров.
  (let ((model (runtime-model :rings 20)))
    (with-test-run (run "live-timing" :model model :snapshot-every 0.002d0)
      (timed-steps run 200)
      (with-test-server (port)
        (let ((without '()) (with '()))
          (dotimes (k 3)
            (push (timed-steps run 600) without)
            (with-live (ws port)
              (ws-send-json ws "type" "rate" "fps" 30)
              (let ((reader (worm.platform:make-thread
                             (lambda () (ignore-errors (loop while (ws-read ws :timeout 2)))))))
                (push (timed-steps run 600) with)
                (ws-close ws)
                (worm.platform:join-thread reader :timeout 5))))
          (let ((a (median without)) (b (median with)))
            (worm.core:log-event :info 'tests "шаг: без клиента ~,3F с, с клиентом ~,3F с на 600 шагов" a b)
            (is (< (/ b a) 1.15d0) "с клиентом ~,3F с против ~,3F с без него" b a)))))))

(defun median (xs) (let ((s (sort (copy-list xs) #'<))) (nth (floor (length s) 2) s)))

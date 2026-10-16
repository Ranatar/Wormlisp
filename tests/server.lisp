;;;; tests/server.lisp — сервер: проверочная страница, маршруты просмотрщика
;;;; и прогонов (этапы 0 и 9). WebSocket /live — tests/live.lisp.

(in-package #:worm.tests)

(def-suite server :in worm :description "HTTP-сервер: маршруты и WebSocket /live.")
(in-suite server)

(defun http-get (port path)
  "Простой GET по HTTP/1.0 на 127.0.0.1: (values код тело)."
  (let ((socket (usocket:socket-connect "127.0.0.1" port :element-type '(unsigned-byte 8))))
    (unwind-protect
         (let ((stream (usocket:socket-stream socket)))
           (write-sequence (sb-ext:string-to-octets
                            (format nil "GET ~A HTTP/1.0~C~CHost: localhost~C~C~C~C"
                                    path #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed)
                            :external-format :utf-8)
                           stream)
           (force-output stream)
           (let* ((bytes (let ((buf (make-array 0 :element-type '(unsigned-byte 8)
                                                  :adjustable t :fill-pointer 0)))
                           (loop for b = (read-byte stream nil)
                                 while b do (vector-push-extend b buf))
                           buf))
                  (octets (coerce bytes '(simple-array (unsigned-byte 8) (*))))
                  (text (handler-case (sb-ext:octets-to-string octets :external-format :utf-8)
                          (error () (sb-ext:octets-to-string octets :external-format :latin-1))))
                  (head-end (search (format nil "~C~C~C~C" #\Return #\Linefeed #\Return #\Linefeed) text)))
             (values (parse-integer text :start 9 :end 12)
                     (if head-end (subseq text (+ head-end 4)) ""))))
      (usocket:socket-close socket))))

(defmacro with-test-server ((port &rest keys) &body body)
  `(let ((,port (+ 18740 (random 1000 (make-random-state t)))))
     (let ((*standard-output* (make-broadcast-stream)))
       (worm:serve :port ,port :address "127.0.0.1" ,@keys))
     (unwind-protect (progn ,@body)
       (worm:stop-serving))))

(test health-answers-ok
  (with-test-server (port)
    (multiple-value-bind (code body) (http-get port "/health")
      (is (= 200 code))
      (is (string= "ok" body)))))

(test check-page-is-served
  (with-test-server (port)
    (multiple-value-bind (code body) (http-get port "/check")
      (is (= 200 code))
      (is (search "Wormlisp на связи" body))
      (is (search "SBCL" body)))))

(test stop-serving-releases-the-port
  (with-test-server (port)
    (is (worm:serving-p))
    (worm:stop-serving)
    (is (not (worm:serving-p)))
    (signals error (http-get port "/health"))))

(test viewer-and-run-files-are-served
  (with-test-run (run "served" :record t)
    (worm.runtime:step-run 30 run)
    (with-test-server (port)
      (multiple-value-bind (code body) (http-get port "/")
        (is (= 200 code)) (is (search "просмотр прогона" body)))
      (is (= 200 (http-get port "/lib/three.min.js")))
      (is (= 200 (http-get port "/selftest-data.js")))
      (is (= 200 (http-get port "/selftest/frames.bin")))
      (multiple-value-bind (code body) (http-get port "/runs")
        (is (= 200 code))
        (let* ((list (worm.io:parse-json body))
               (entry (find (worm.runtime:run-id run) list :key (lambda (h) (gethash "id" h)) :test #'equal)))
          (is (not (null entry)) "живой прогон в списке")
          (is (and entry (eq t (gethash "live" entry))))
          (is (and entry (equal "worm-3d" (gethash "model" entry))))))
      (let ((base (format nil "/runs/~A/" (worm.runtime:run-id run))))
        (multiple-value-bind (code body) (http-get port (concatenate 'string base "meta.json"))
          (is (= 200 code))
          (is (equal "agar" (gethash "medium" (worm.io:parse-json body)))))
        (is (= 200 (http-get port (concatenate 'string base "frames.bin"))))
        (is (= 404 (http-get port (concatenate 'string base "secret.txt"))))
        (is (= 404 (http-get port "/runs/../worm.asd/meta.json")))
        (is (= 404 (http-get port "/runs/no-such-run/meta.json")))
        (is (= 404 (http-get port "/selftest/../../worm.asd")))))))

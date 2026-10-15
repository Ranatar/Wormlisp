;;;; tests/server.lisp — проверочная страница отвечает по HTTP.

(in-package #:worm.tests)

(def-suite server :in worm :description "HTTP-сервер этапа 0.")
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
                  (text (sb-ext:octets-to-string (coerce bytes '(simple-array (unsigned-byte 8) (*)))
                                                 :external-format :utf-8))
                  (head-end (search (format nil "~C~C~C~C" #\Return #\Linefeed #\Return #\Linefeed) text)))
             (values (parse-integer text :start 9 :end 12)
                     (if head-end (subseq text (+ head-end 4)) ""))))
      (usocket:socket-close socket))))

(defmacro with-test-server ((port) &body body)
  `(let ((,port (+ 18740 (random 1000 (make-random-state t)))))
     (worm:serve :port ,port :address "127.0.0.1")
     (unwind-protect (progn ,@body)
       (worm:stop-serving))))

(test health-answers-ok
  (with-test-server (port)
    (multiple-value-bind (code body) (http-get port "/health")
      (is (= 200 code))
      (is (string= "ok" body)))))

(test check-page-is-served
  (with-test-server (port)
    (multiple-value-bind (code body) (http-get port "/")
      (is (= 200 code))
      (is (search "Wormlisp на связи" body))
      (is (search "SBCL" body)))))

(test stop-serving-releases-the-port
  (with-test-server (port)
    (is (worm:serving-p))
    (worm:stop-serving)
    (is (not (worm:serving-p)))
    (signals error (http-get port "/health"))))

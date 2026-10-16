;;;; scripts/make-selftest.lisp — эталон для самопроверки просмотрщика.
;;;;
;;;; Пишет:
;;;;   tests/data/frames.bin            — небольшой файл кадров известного содержания;
;;;;   tests/data/frames-expected.json  — что должно получиться при разборе;
;;;;   viewer/selftest-data.js          — то же для просмотрщика: файл в base64
;;;;                                      и ожидаемые значения (тег <script>
;;;;                                      работает и при открытии с диска).
;;;;
;;;; Запуск из корня репозитория:
;;;;   sbcl --non-interactive --no-userinit --load scripts/init.lisp --load scripts/make-selftest.lisp
;;;;
;;;; Содержание кадров задано формулами, а ожидаемые значения, включая
;;;; кривизну колец, считает Лисп: так формат кадра и разбор кривизны
;;;; проверяются с обеих сторон (архитектура, «Самопроверка»).

(asdf:load-system "worm/io")
(asdf:load-system "worm/topology")
(asdf:load-system "cl-base64")

(defpackage #:worm.selftest (:use #:cl #:worm.core))
(in-package #:worm.selftest)

(defparameter *rings* 6)
(defparameter *ring-nodes* 8)
(defparameter *muscles* 95)
(defparameter *frames* 5)

(defun frame-coords (k)
  "Трубка по дуге с кривизной, меняющейся от кадра к кадру, со сдвигом в
отрицательные координаты — чтобы проверить знак и порядок байтов."
  (let* ((tube (worm.topology:make-tube :rings *rings* :ring-nodes *ring-nodes*))
         (kappa (- (* 1.5d0 k) 3d0))
         (rest (worm.topology:tube-rest-coords tube))
         (s-vec (worm.topology:tube-ring-s tube))
         (out (make-dvec (length rest))))
    (dotimes (i *rings* out)
      (let* ((s (aref s-vec i))
             (phi (* kappa s))
             (cx (if (zerop kappa) s (/ (sin phi) kappa)))
             (cy (if (zerop kappa) 0d0 (/ (- 1d0 (cos phi)) kappa))))
        (dotimes (j *ring-nodes*)
          (let* ((p (* 3 (+ (* i *ring-nodes*) j)))
                 (oy (aref rest (+ p 1))) (oz (aref rest (+ p 2))))
            (setf (aref out p) (- (- cx (* (sin phi) oy)) 0.25d0 (* 0.01d0 k))
                  (aref out (+ p 1)) (+ cy (* (cos phi) oy) -0.5d0)
                  (aref out (+ p 2)) (+ oz 0.125d0))))))))

(defun frame-activations (k)
  (let ((a (make-dvec *muscles*)))
    (dotimes (j *muscles* a)
      (setf (aref a j) (/ (mod (+ (* 7 j) (* 13 k)) 101) 100d0)))))

(defun frame-step (k) (+ (expt 2 40) (* 1000 k)))
(defun frame-time (k) (+ 0.1d0 (* 0.02d0 k)))

(defun single-precision (v)
  "Значения такими, какими их увидит читатель кадра: через single-float."
  (map 'dvec (lambda (x) (float (float x 0f0) 0d0)) v))

(let* ((root (project-root))
       (bin (merge-pathnames "tests/data/frames.bin" root))
       (n (* *rings* *ring-nodes*))
       (writer (worm.io:open-frames-writer bin n *muscles*))
       (expected-frames '()))
  (dotimes (k *frames*)
    (let ((coords (frame-coords k)) (acts (frame-activations k)))
      (worm.io:write-frame writer (frame-step k) (frame-time k) coords acts)
      (let* ((seen (single-precision coords))
             (curv (make-dvec *rings*)))
        (worm.topology:ring-curvatures seen *rings* *ring-nodes* (make-dvec (* 3 *rings*)) curv)
        (push (worm.io:json-object
               "step" (frame-step k) "time" (frame-time k)
               "firstCoord" (aref seen 0) "lastCoord" (aref seen (1- (length seen)))
               "coordSum" (reduce #'+ seen)
               "firstActivation" (float (float (aref acts 0) 0f0) 0d0)
               "lastActivation" (float (float (aref acts (1- *muscles*)) 0f0) 0d0)
               "curvature" (coerce curv 'vector))
              expected-frames))))
  (worm.io:close-frames-writer writer)
  (let* ((expected (worm.io:json-object
                    "nodes" n "muscles" *muscles* "neurons" 0 "rings" *rings* "ringNodes" *ring-nodes*
                    "frameSize" (worm.io:frame-size n *muscles*) "count" *frames*
                    "frames" (coerce (nreverse expected-frames) 'vector)))
         (octets (with-open-file (in bin :element-type '(unsigned-byte 8))
                   (let ((b (make-array (file-length in) :element-type '(unsigned-byte 8))))
                     (read-sequence b in) b)))
         (base64 (cl-base64:usb8-array-to-base64-string octets)))
    (worm.io:write-json-file (merge-pathnames "tests/data/frames-expected.json" root) expected)
    (with-open-file (out (merge-pathnames "viewer/selftest-data.js" root)
                         :direction :output :if-exists :supersede :if-does-not-exist :create
                         :external-format :utf-8)
      (format out "// viewer/selftest-data.js — порождён scripts/make-selftest.lisp, не править руками.~%")
      (format out "// Эталонный tests/data/frames.bin в base64 и ожидаемые значения разбора.~%")
      (format out "window.WORM_SELFTEST = {~%  framesBase64: ~S,~%  expected: " base64)
      (com.inuoe.jzon:stringify expected :stream out)
      (format out "~%};~%"))
    (format t "~&Записаны ~A, frames-expected.json и viewer/selftest-data.js (~D байт кадров)~%"
            bin (length octets))))

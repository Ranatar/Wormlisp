;;;; tests/io.lisp — кадр, frames.bin, контрольная точка на диске, JSON,
;;;; каталог прогона и эталон самопроверки просмотрщика (этап 4).

(in-package #:worm.tests)

(def-suite io :in worm :description "Форматы файлов прогона.")
(in-suite io)

(defun scratch-path (name)
  "Путь во временном каталоге тестов cache/test-io/ (каталог не в git)."
  (let ((p (merge-pathnames (concatenate 'string "cache/test-io/" name) (worm.core:project-root))))
    (ensure-directories-exist p)
    p))

(defun file-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence b in)
      b)))

(defun write-octets (path octets)
  (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :supersede :if-does-not-exist :create)
    (write-sequence octets out))
  path)

(defun single-round (x) (float (float x 0f0) 0d0))

(defun sample-coords (n k)
  (let ((v (worm.core:make-dvec (* 3 n))))
    (dotimes (i (* 3 n) v)
      (setf (aref v i) (+ (* 0.37d0 (sin (+ i k))) (* -1d-3 i))))))

(test frame-round-trip
  (let* ((n 7) (m 5) (q 3)
         (coords (sample-coords n 2))
         (acts (worm.core:dvec-from '(0d0 0.25d0 0.5d0 1d0 0.1d0)))
         (neurons (worm.core:dvec-from '(-65d0 -40.5d0 12.125d0)))
         (size (worm.io:frame-size n m q))
         (buf (make-array (+ 9 size) :element-type '(unsigned-byte 8) :initial-element 0))
         (end (worm.io:encode-frame buf 9 (expt 2 40) 0.123456789012345d0 coords acts neurons))
         (frame (worm.io:decode-frame buf 9 q)))
    (is (= (+ 9 size) end) "кадр занимает ровно frame-size байт")
    (is (= (expt 2 40) (worm.io:frame-step frame)) "шаг u64 больше 2³²")
    (is (= 0.123456789012345d0 (worm.io:frame-time frame)) "время f64 без потерь")
    (is (every (lambda (a b) (= (single-round a) b)) coords (worm.io:frame-coords frame))
        "координаты — ровно single-float")
    (is (every (lambda (a b) (= (single-round a) b)) acts (worm.io:frame-activations frame)))
    (is (every (lambda (a b) (= (single-round a) b)) neurons (worm.io:frame-neurons frame))
        "потенциалы нейронов читаются по флагу")
    (setf (aref buf 9) 0)
    (signals worm.io:format-version-mismatch (worm.io:decode-frame buf 9 q))))

(test frames-file-round-trip
  (let* ((path (scratch-path "frames.bin")) (n 12) (m 4) (frames 6)
         (writer (worm.io:open-frames-writer path n m)))
    (dotimes (k frames)
      (worm.io:write-frame writer (* 10 k) (* 0.01d0 k) (sample-coords n k)
                           (worm.core:dvec-from (list 0d0 (* 0.1d0 k) 0.5d0 1d0))))
    (worm.io:close-frames-writer writer)
    (let ((file (worm.io:read-frames-file path)))
      (is (= frames (worm.io:frames-file-count file)))
      (is (= n (worm.io:frames-file-nodes file)))
      (is (= m (worm.io:frames-file-muscles file)))
      (is (= (+ worm.io:+file-header-size+ (* frames (worm.io:frame-size n m)))
             (length (file-octets path))))
      (let ((f (worm.io:frame-ref file 4)))
        (is (= 40 (worm.io:frame-step f)))
        (is (= 0.04d0 (worm.io:frame-time f)))
        (is (= (single-round 0.4d0) (aref (worm.io:frame-activations f) 1)))
        (is (every (lambda (a b) (= (single-round a) b)) (sample-coords n 4) (worm.io:frame-coords f))))
      (signals error (worm.io:frame-ref file frames)))))

(test interrupted-run-recovers-frame-count
  ;; Оборванный прогон: заголовок не обновлён (число кадров 0), последний
  ;; кадр записан не до конца. Читаются только целые кадры.
  (let* ((path (scratch-path "frames-cut.bin")) (n 5) (m 2)
         (writer (worm.io:open-frames-writer path n m)))
    (dotimes (k 4)
      (worm.io:write-frame writer k (* 0.5d0 k) (sample-coords n k) (worm.core:dvec-from '(0d0 1d0))))
    (worm.io:close-frames-writer writer)
    (let* ((octets (file-octets path))
           (cut (subseq octets 0 (+ worm.io:+file-header-size+ (* 3 (worm.io:frame-size n m)) 11))))
      (fill cut 0 :start 24 :end 32)
      (write-octets path cut)
      (let ((file (worm.io:read-frames-file path)))
        (is (= 3 (worm.io:frames-file-count file)))
        (is (= 1d0 (worm.io:frame-time (worm.io:frame-ref file 2)))))
      (write-octets path (subseq octets 0 10))
      (signals worm.io:format-version-mismatch (worm.io:read-frames-file path)))))

(test checkpoint-file-round-trip
  (let* ((path (scratch-path "checkpoint.bin"))
         (y (sample-coords 9 1))
         (z (worm.core:dvec-from '(1d0 -2d0 3.5d0))))
    (worm.io:write-checkpoint-file path :model-hash "0123456789abcdef0123456789abcdef"
                                        :layout-hash "layout-1" :step 123456789012 :time 2.5d0
                                        :h 1d-4 :vectors (list (cons "y-mech" y) (cons "z" z)))
    (let ((c (worm.io:read-checkpoint-file path :model-hash "0123456789abcdef0123456789abcdef")))
      (is (= 123456789012 (getf c :step)))
      (is (= 2.5d0 (getf c :time)))
      (is (= 1d-4 (getf c :h)))
      (is (string= "layout-1" (getf c :layout-hash)))
      (is (equal '("y-mech" "z") (mapcar #'car (getf c :vectors))))
      (is (equalp y (cdr (first (getf c :vectors)))) "векторы double без потерь")
      (is (equalp z (cdr (second (getf c :vectors))))))
    (signals worm.io:checkpoint-mismatch
      (worm.io:read-checkpoint-file path :model-hash "ffffffffffffffffffffffffffffffff"))
    (signals worm.io:checkpoint-mismatch
      (worm.io:read-checkpoint-file path :layout-hash "layout-2"))
    (write-octets path (make-array 100 :element-type '(unsigned-byte 8) :initial-element 0))
    (signals worm.io:format-version-mismatch (worm.io:read-checkpoint-file path))))

(test json-round-trip
  (let ((path (scratch-path "meta.json")))
    (worm.io:write-json-file path (worm.io:json-object
                                   "name" "кольцо" "n" 50 "x" 0.25d0 "yes" t "no" nil
                                   "nothing" 'null "list" #(1 2 3)
                                   "nested" (worm.io:json-object "a" "b")))
    (let ((h (worm.io:read-json-file path)))
      (is (string= "кольцо" (gethash "name" h)) "UTF-8")
      (is (eql 50 (gethash "n" h)))
      (is (= 0.25d0 (gethash "x" h)))
      (is (eq t (gethash "yes" h)))
      (is (null (gethash "no" h)))
      (is (eq 'null (gethash "nothing" h)) "null, а не строка «NULL»")
      (is (equalp #(1 2 3) (gethash "list" h)))
      (is (string= "b" (gethash "a" (gethash "nested" h)))))))

(test run-directory
  (let* ((root (merge-pathnames "cache/test-io/runs/" (worm.core:project-root)))
         (id (worm.io:make-run-id "test"))
         (dir (worm.io:make-run-directory :id id :root root)))
    (is (cl-ppcre:scan "^\\d{8}-\\d{6}-[0-9a-f]{6}$" id) "идентификатор: ~A" id)
    (is (string= "running" (worm.io:read-status dir)))
    (worm.io:write-status dir "done")
    (is (string= "done" (worm.io:read-status dir)))))

(test viewer-selftest-data-is-current
  ;; viewer/selftest-data.js порождается scripts/make-selftest.lisp из
  ;; tests/data/frames.bin; эталоны не должны разойтись.
  (let* ((root (worm.core:project-root))
         (bin (merge-pathnames "tests/data/frames.bin" root))
         (expected (worm.io:read-json-file (merge-pathnames "tests/data/frames-expected.json" root)))
         (js (with-open-file (in (merge-pathnames "viewer/selftest-data.js" root) :external-format :utf-8)
               (let ((s (make-string (file-length in))))
                 (subseq s 0 (read-sequence s in)))))
         (file (worm.io:read-frames-file bin)))
    (is (search (cl-base64:usb8-array-to-base64-string (file-octets bin)) js)
        "base64 в selftest-data.js совпадает с tests/data/frames.bin")
    (is (= (gethash "count" expected) (worm.io:frames-file-count file)))
    (is (= (gethash "nodes" expected) (worm.io:frames-file-nodes file)))
    (loop for e across (gethash "frames" expected)
          for k from 0
          for f = (worm.io:frame-ref file k)
          for coords = (worm.core:dvec-from (worm.io:frame-coords f))
          for curv = (worm.core:make-dvec (gethash "rings" expected))
          do (is (= (gethash "step" e) (worm.io:frame-step f)))
             (is (= (gethash "time" e) (worm.io:frame-time f)))
             (worm.topology:ring-curvatures coords (gethash "rings" expected) (gethash "ringNodes" expected)
                                            (worm.core:make-dvec (* 3 (gethash "rings" expected))) curv)
             (is (every (lambda (a b) (< (abs (- a b)) 1d-12)) curv (gethash "curvature" e))
                 "кривизна кадра ~D как в эталоне" k))))

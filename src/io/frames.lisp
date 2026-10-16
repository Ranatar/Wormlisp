;;;; src/io/frames.lisp — кадр и файл кадров.
;;;;
;;;; Кадр (архитектура, «Кадр»), little-endian:
;;;;
;;;;   0          u32       сигнатура WRMF
;;;;   4          u16       версия формата
;;;;   6          u16       флаги: бит 0 — есть потенциалы нейронов
;;;;   8          u64       номер шага
;;;;   16         f64       время, с
;;;;   24         u32       число узлов n
;;;;   28         u32       число мышц m
;;;;   32         f32 × 3n  координаты узлов, мм
;;;;   32 + 12n   f32 × m   активации мышц, 0…1
;;;;   далее      f32 × q   потенциалы нейронов (если флаг 0)
;;;;
;;;; frames.bin — заголовок файла (64 байта) и кадры постоянного размера:
;;;;
;;;;   0  u32 сигнатура WRMS     4  u16 версия      6  u16 флаги
;;;;   8  u32 n                 12  u32 m          16  u32 q
;;;;   20 u32 размер кадра      24  u64 число кадров
;;;;   32…63 нули (резерв)
;;;;
;;;; Число кадров обновляется при закрытии; если прогон оборвался, оно
;;;; восстанавливается по длине файла.

(in-package #:worm.io)

(defconstant +frame-version+ 1)
(defconstant +file-header-size+ 64)
(defparameter +frame-signature+ #x464d5257 "«WRMF» как u32 little-endian.")
(defparameter +file-signature+ #x534d5257 "«WRMS» как u32 little-endian.")

(define-condition format-version-mismatch (io-error) ())
(define-condition checkpoint-mismatch (io-error) ())

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

;;; Числа little-endian

(declaim (inline put-u16 put-u32 put-u64 put-f32 put-f64 get-u16 get-u32 get-u64 get-f32 get-f64))

(defun put-u16 (buf pos v)
  (declare (type octets buf) (type index pos) (type (unsigned-byte 16) v))
  (setf (aref buf pos) (ldb (byte 8 0) v) (aref buf (+ pos 1)) (ldb (byte 8 8) v)))

(defun put-u32 (buf pos v)
  (declare (type octets buf) (type index pos) (type (unsigned-byte 32) v))
  (dotimes (i 4) (setf (aref buf (+ pos i)) (ldb (byte 8 (* 8 i)) v))))

(defun put-u64 (buf pos v)
  (declare (type octets buf) (type index pos) (type (unsigned-byte 64) v))
  (dotimes (i 8) (setf (aref buf (+ pos i)) (ldb (byte 8 (* 8 i)) v))))

(defun put-f32 (buf pos x)
  (declare (type octets buf) (type index pos) (type single-float x))
  (put-u32 buf pos (ldb (byte 32 0) (sb-kernel:single-float-bits x))))

(defun put-f64 (buf pos x)
  (declare (type octets buf) (type index pos) (type double-float x))
  (put-u32 buf pos (sb-kernel:double-float-low-bits x))
  (put-u32 buf (+ pos 4) (ldb (byte 32 0) (sb-kernel:double-float-high-bits x))))

(defun get-u16 (buf pos)
  (declare (type octets buf) (type index pos))
  (logior (aref buf pos) (ash (aref buf (+ pos 1)) 8)))

(defun get-u32 (buf pos)
  (declare (type octets buf) (type index pos))
  (logior (aref buf pos) (ash (aref buf (+ pos 1)) 8)
          (ash (aref buf (+ pos 2)) 16) (ash (aref buf (+ pos 3)) 24)))

(defun get-u64 (buf pos)
  (declare (type octets buf) (type index pos))
  (logior (get-u32 buf pos) (ash (get-u32 buf (+ pos 4)) 32)))

(defun signed-32 (u) (if (logbitp 31 u) (- u (ash 1 32)) u))

(defun get-f32 (buf pos)
  (declare (type octets buf) (type index pos))
  (sb-kernel:make-single-float (signed-32 (get-u32 buf pos))))

(defun get-f64 (buf pos)
  (declare (type octets buf) (type index pos))
  (sb-kernel:make-double-float (signed-32 (get-u32 buf (+ pos 4))) (get-u32 buf pos)))

;;; Кадр

(defun frame-size (n m &optional (q 0))
  "Размер кадра в байтах при N узлах, M мышцах и Q нейронах."
  (+ 32 (* 12 n) (* 4 m) (* 4 q)))

(defstruct (frame (:copier nil))
  (step 0 :type (unsigned-byte 64))
  (time 0d0 :type double-float)
  (coords nil :type fvec)
  (activations nil :type fvec)
  (neurons nil :type (or null fvec)))

(defun encode-frame (buf pos step time coords activations &optional neurons)
  "Записать кадр в BUF с позиции POS. COORDS — 3n координат (dvec или fvec),
ACTIVATIONS — m чисел, NEURONS — q чисел или NIL. Возвращает позицию за кадром."
  (declare (type octets buf) (type index pos))
  (let ((n (floor (length coords) 3))
        (m (length activations)))
    (put-u32 buf pos +frame-signature+)
    (put-u16 buf (+ pos 4) +frame-version+)
    (put-u16 buf (+ pos 6) (if neurons 1 0))
    (put-u64 buf (+ pos 8) step)
    (put-f64 buf (+ pos 16) (float time 0d0))
    (put-u32 buf (+ pos 24) n)
    (put-u32 buf (+ pos 28) m)
    (let ((p (+ pos 32)))
      (declare (type index p))
      (map nil (lambda (x) (put-f32 buf p (float x 0f0)) (incf p 4)) coords)
      (map nil (lambda (x) (put-f32 buf p (float x 0f0)) (incf p 4)) activations)
      (when neurons
        (map nil (lambda (x) (put-f32 buf p (float x 0f0)) (incf p 4)) neurons))
      p)))

(defun decode-frame (buf pos &optional (q 0))
  "Прочитать кадр из BUF с позиции POS. Число нейронов Q кадр не хранит —
оно берётся из заголовка файла; потенциалы читаются, если стоит флаг."
  (declare (type octets buf) (type index pos))
  (unless (= (get-u32 buf pos) +frame-signature+)
    (error 'format-version-mismatch :message (format nil "нет сигнатуры кадра WRMF на смещении ~D" pos)))
  (unless (= (get-u16 buf (+ pos 4)) +frame-version+)
    (error 'format-version-mismatch
           :message (format nil "версия кадра ~D, а читатель знает ~D" (get-u16 buf (+ pos 4)) +frame-version+)))
  (let* ((flags (get-u16 buf (+ pos 6)))
         (n (get-u32 buf (+ pos 24)))
         (m (get-u32 buf (+ pos 28)))
         (coords (make-fvec (* 3 n)))
         (acts (make-fvec m))
         (p (+ pos 32)))
    (dotimes (i (* 3 n)) (setf (aref coords i) (get-f32 buf p)) (incf p 4))
    (dotimes (i m) (setf (aref acts i) (get-f32 buf p)) (incf p 4))
    (make-frame :step (get-u64 buf (+ pos 8)) :time (get-f64 buf (+ pos 16))
                :coords coords :activations acts
                :neurons (when (and (logbitp 0 flags) (plusp q))
                           (let ((v (make-fvec q)))
                             (dotimes (i q v) (setf (aref v i) (get-f32 buf p)) (incf p 4)))))))

;;; Запись frames.bin

(defstruct (frames-writer (:copier nil))
  (path nil)
  (stream nil)
  (n 0 :type index) (m 0 :type index) (q 0 :type index)
  (buffer nil :type octets)
  (count 0 :type index))

(defun write-file-header (stream n m q count)
  (let ((h (make-array +file-header-size+ :element-type '(unsigned-byte 8) :initial-element 0)))
    (put-u32 h 0 +file-signature+)
    (put-u16 h 4 +frame-version+)
    (put-u16 h 6 (if (plusp q) 1 0))
    (put-u32 h 8 n) (put-u32 h 12 m) (put-u32 h 16 q)
    (put-u32 h 20 (frame-size n m q))
    (put-u64 h 24 count)
    (file-position stream 0)
    (write-sequence h stream)))

(defun open-frames-writer (path n m &key (q 0))
  "Создать frames.bin для N узлов, M мышц и Q нейронов."
  (ensure-directories-exist path)
  (let ((stream (open path :direction :output :element-type '(unsigned-byte 8)
                           :if-exists :supersede :if-does-not-exist :create)))
    (write-file-header stream n m q 0)
    (make-frames-writer :path path :stream stream :n n :m m :q q
                        :buffer (make-array (frame-size n m q) :element-type '(unsigned-byte 8)))))

(defun write-frame (writer step time coords activations &optional neurons)
  "Дописать кадр. Буфер кадра выделен один раз при открытии."
  (let ((buf (frames-writer-buffer writer)))
    (unless (= (length coords) (* 3 (frames-writer-n writer)))
      (error "кадр: ~D координат, а файл ждёт ~D" (length coords) (* 3 (frames-writer-n writer))))
    (unless (= (length activations) (frames-writer-m writer))
      (error "кадр: ~D активаций, а файл ждёт ~D" (length activations) (frames-writer-m writer)))
    (encode-frame buf 0 step time coords activations
                  (and (plusp (frames-writer-q writer)) neurons))
    (write-sequence buf (frames-writer-stream writer))
    (incf (frames-writer-count writer))
    writer))

(defun close-frames-writer (writer)
  "Записать число кадров в заголовок и закрыть файл."
  (let ((stream (frames-writer-stream writer)))
    (when (open-stream-p stream)
      (write-file-header stream (frames-writer-n writer) (frames-writer-m writer)
                         (frames-writer-q writer) (frames-writer-count writer))
      (close stream)))
  writer)

;;; Чтение frames.bin

(defstruct (frames-file (:copier nil))
  (path nil)
  (octets nil :type octets)
  (nodes 0 :type index) (muscles 0 :type index) (neurons 0 :type index)
  (frame-size 0 :type index)
  (count 0 :type index))

(defun read-frames-file (path)
  "Прочитать frames.bin целиком. Если число кадров в заголовке не записано
(прогон оборвался), оно восстанавливается по длине файла."
  (let ((octets (with-open-file (in path :element-type '(unsigned-byte 8))
                  (let ((buf (make-array (file-length in) :element-type '(unsigned-byte 8))))
                    (read-sequence buf in)
                    buf))))
    (when (or (< (length octets) +file-header-size+) (/= (get-u32 octets 0) +file-signature+))
      (error 'format-version-mismatch :message (format nil "~A — не файл кадров WRMS" path)))
    (unless (= (get-u16 octets 4) +frame-version+)
      (error 'format-version-mismatch
             :message (format nil "~A: версия ~D, а читатель знает ~D" path (get-u16 octets 4) +frame-version+)))
    (let* ((n (get-u32 octets 8)) (m (get-u32 octets 12)) (q (get-u32 octets 16))
           (size (get-u32 octets 20))
           (stored (get-u64 octets 24))
           (complete (floor (- (length octets) +file-header-size+) size)))
      (unless (= size (frame-size n m q))
        (error 'format-version-mismatch :message (format nil "~A: размер кадра ~D не сходится с n, m, q" path size)))
      (make-frames-file :path path :octets octets :nodes n :muscles m :neurons q :frame-size size
                        :count (if (and (plusp stored) (<= stored complete)) stored complete)))))

(defun frame-ref (file k)
  "Кадр номер K (с нуля) — одним смещением, без индекса."
  (unless (< -1 k (frames-file-count file))
    (error "кадра ~D нет: в файле ~D кадров" k (frames-file-count file)))
  (decode-frame (frames-file-octets file)
                (+ +file-header-size+ (* k (frames-file-frame-size file)))
                (frames-file-neurons file)))

;;;; src/io/run.lisp — meta.json, каталог прогона, контрольные точки на диске.
;;;;
;;;; Каталог прогона runs/<id>/ (архитектура, «Хранилище и форматы файлов»):
;;;; config.sexp, meta.json, frames.bin, metrics.csv, checkpoints/, log.txt,
;;;; status. Идентификатор интерактивного прогона — дата-время UTC и
;;;; короткий хэш.

(in-package #:worm.io)

;;; JSON

(defun json-object (&rest pairs)
  "Объект JSON из пар ключ — значение; ключи — строки. Значения: числа,
строки, T/NIL как true/false, символ NULL как null, векторы, вложенные объекты."
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr do (setf (gethash k h) v))
    h))

(defun write-json-file (path object)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (com.inuoe.jzon:stringify object :stream out :pretty t)
    (terpri out))
  path)

(defun read-json-file (path)
  (with-open-file (in path :external-format :utf-8)
    (com.inuoe.jzon:parse in)))

;;; Каталог прогона

(defun runs-directory ()
  (merge-pathnames "runs/" (project-root)))

(defun make-run-id (&optional (salt ""))
  "Идентификатор вида 20261016-073012-1a2b3c: время UTC и шесть знаков хэша."
  (multiple-value-bind (s mi h d mo y) (decode-universal-time (get-universal-time) 0)
    (let ((stamp (format nil "~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D" y mo d h mi s)))
      (format nil "~A-~A" stamp
              (subseq (worm.platform:md5-hex (format nil "~A ~A ~A" stamp salt (random 1000000000)))
                      0 6)))))

(defun make-run-directory (&key (id (make-run-id)) (root (runs-directory)))
  "Создать каталог прогона и вернуть его путь."
  (let ((dir (merge-pathnames (format nil "~A/" id) root)))
    (ensure-directories-exist dir)
    (write-status dir "running")
    dir))

(defun run-file (dir name)
  (merge-pathnames name dir))

(defun write-status (dir status)
  (with-open-file (out (run-file dir "status") :direction :output :if-exists :supersede
                                               :if-does-not-exist :create)
    (write-line status out)))

(defun read-status (dir)
  (with-open-file (in (run-file dir "status") :if-does-not-exist nil)
    (and in (string-trim '(#\Space #\Newline #\Return) (read-line in nil "")))))

;;; Контрольная точка на диске
;;;
;;;   0  u32 «WRMC»   4 u16 версия   6 u16 число векторов
;;;   8  32 байта хэш модели (ASCII)   40 32 байта хэш раскладки
;;;   72 u64 шаг   80 f64 время   88 f64 h
;;;   96… векторы: u32 длина имени, имя (ASCII), u32 число элементов, f64 × число

(defparameter +checkpoint-signature+ #x434d5257 "«WRMC».")
(defconstant +checkpoint-version+ 1)

(defun ascii-bytes (string length)
  (let ((out (make-array length :element-type '(unsigned-byte 8) :initial-element 32)))
    (loop for ch across (subseq string 0 (min length (length string)))
          for i from 0 do (setf (aref out i) (char-code ch)))
    out))

(defun write-checkpoint-file (path &key model-hash layout-hash (step 0) (time 0d0) (h 0d0) vectors)
  "Записать контрольную точку. VECTORS — список (имя . dvec)."
  (ensure-directories-exist path)
  (let* ((size (+ 96 (loop for (name . v) in vectors sum (+ 8 (length name) (* 8 (length v))))))
         (buf (make-array size :element-type '(unsigned-byte 8) :initial-element 0))
         (p 96))
    (put-u32 buf 0 +checkpoint-signature+)
    (put-u16 buf 4 +checkpoint-version+)
    (put-u16 buf 6 (length vectors))
    (replace buf (ascii-bytes (or model-hash "") 32) :start1 8)
    (replace buf (ascii-bytes (or layout-hash "") 32) :start1 40)
    (put-u64 buf 72 step)
    (put-f64 buf 80 (float time 0d0))
    (put-f64 buf 88 (float h 0d0))
    (loop for (name . v) in vectors
          do (put-u32 buf p (length name)) (incf p 4)
             (replace buf (ascii-bytes name (length name)) :start1 p) (incf p (length name))
             (put-u32 buf p (length v)) (incf p 4)
             (loop for x across v do (put-f64 buf p (float x 0d0)) (incf p 8)))
    (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                              :if-exists :supersede :if-does-not-exist :create)
      (write-sequence buf out))
    path))

(defun read-checkpoint-file (path &key model-hash layout-hash)
  "Прочитать контрольную точку: plist (:model-hash :layout-hash :step :time :h :vectors).
Если задан MODEL-HASH и он не совпал — ошибка checkpoint-mismatch: точку от
другой модели загрузить нельзя (архитектура, «Контрольная точка»)."
  (let ((buf (with-open-file (in path :element-type '(unsigned-byte 8))
               (let ((b (make-array (file-length in) :element-type '(unsigned-byte 8))))
                 (read-sequence b in) b))))
    (unless (and (>= (length buf) 96) (= (get-u32 buf 0) +checkpoint-signature+))
      (error 'format-version-mismatch :message (format nil "~A — не контрольная точка WRMC" path)))
    (unless (= (get-u16 buf 4) +checkpoint-version+)
      (error 'format-version-mismatch :message (format nil "~A: версия ~D" path (get-u16 buf 4))))
    (flet ((ascii (start len) (string-right-trim " " (map 'string #'code-char (subseq buf start (+ start len))))))
      (let ((stored-model (ascii 8 32)) (stored-layout (ascii 40 32)) (p 96) (vectors '()))
        (when (and model-hash (string/= model-hash stored-model))
          (error 'checkpoint-mismatch
                 :message (format nil "контрольная точка от модели ~A, а загружена ~A" stored-model model-hash)))
        (when (and layout-hash (string/= layout-hash stored-layout))
          (error 'checkpoint-mismatch
                 :message (format nil "контрольная точка с раскладкой ~A, а у модели ~A" stored-layout layout-hash)))
        (dotimes (k (get-u16 buf 6))
          (let* ((name-len (get-u32 buf p))
                 (name (map 'string #'code-char (subseq buf (+ p 4) (+ p 4 name-len)))))
            (incf p (+ 4 name-len))
            (let* ((n (get-u32 buf p)) (v (make-dvec n)))
              (incf p 4)
              (dotimes (i n) (setf (aref v i) (get-f64 buf p)) (incf p 8))
              (push (cons name v) vectors))))
        (list :model-hash stored-model :layout-hash stored-layout
              :step (get-u64 buf 72) :time (get-f64 buf 80) :h (get-f64 buf 88)
              :vectors (nreverse vectors))))))

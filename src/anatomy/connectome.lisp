;;;; src/anatomy/connectome.lisp — нейроны и синапсы третьего уровня (этап 12).
;;;;
;;;; Формат anatomy/neurons.sexp:
;;;;
;;;;   (connectome-source :name "..." :file "anatomy/source/..." :origin "..." :md5 "...")
;;;;   (neuron vb06 :class :vb :transmitter :acetylcholine :position 0.52 :nmj 29)
;;;;
;;;; Формат anatomy/synapses.sexp:
;;;;
;;;;   (synapse vb06 dd03 :contacts 4)   ; химический синапс пре → пост
;;;;   (gap     vb06 vb07 :contacts 2)   ; щелевой контакт, одна запись на пару
;;;;   (nmj     vb06 mvl11 :contacts 3)  ; нервно-мышечный синапс
;;;;
;;;; Положение нейрона — доля длины тела: средняя середина мышц, которые он
;;;; иннервирует, с весами числа контактов; у нейронов без нервно-мышечных
;;;; синапсов — NIL. Файлы порождает scripts/convert-connectome.lisp из
;;;; опубликованной таблицы; источник и его MD5 записаны в первой форме и
;;;; переходят в meta.json каждого прогона.

(in-package #:worm.anatomy)

;; структуры neuron и connectome — в muscles.lisp: anatomy.lisp обращается к
;; ним раньше, чем грузится этот файл.

(defun neuron-index (connectome name)
  (position name (connectome-neurons connectome) :key #'neuron-name))

(defun parse-neuron (form)
  (destructuring-bind (head name &rest plist) form
    (declare (ignore head))
    (unless (and name (symbolp name)) (invalid "у нейрона нет имени: ~S" form))
    (let ((class (getf plist :class))
          (tr (getf plist :transmitter :acetylcholine))
          (pos (getf plist :position)))
      (unless (and class (symbolp class)) (invalid "~(~A~): класс ~S" name class))
      (unless (member tr +transmitters+) (invalid "~(~A~): медиатор ~S не из ~{~(~S~)~^ ~}" name tr +transmitters+))
      (unless (or (null pos) (and (realp pos) (<= 0 pos 1))) (invalid "~(~A~): положение ~S" name pos))
      (make-neuron :name (intern (symbol-name name) :keyword)
                   :class (intern (symbol-name class) :keyword)
                   :transmitter tr
                   :position (and pos (float pos 0d0))
                   :nmj (getf plist :nmj 0)))))

(defun read-connectome (neurons-path synapses-path muscles)
  "Нейроны и синапсы из файлов; MUSCLES — вектор мышц анатомии для nmj."
  (let ((source nil) (neurons '()))
    (dolist (form (read-forms neurons-path))
      (cond ((head-is form "CONNECTOME-SOURCE") (setf source (rest form)))
            ((head-is form "NEURON") (push (parse-neuron form) neurons))
            (t (invalid "неизвестная форма в файле нейронов: ~S" form))))
    (let* ((neurons (coerce (nreverse neurons) 'simple-vector))
           (index (make-hash-table :test 'eq))
           (chemical '()) (gap '()) (nmj '()))
      (loop for n across neurons for i from 0
            do (when (gethash (neuron-name n) index) (invalid "нейрон ~(~A~) повторяется" (neuron-name n)))
               (setf (gethash (neuron-name n) index) i))
      (flet ((neuron (name form)
               (or (gethash (intern (symbol-name name) :keyword) index)
                   (invalid "~S: нейрона ~(~A~) нет в файле нейронов" form name)))
             (contacts (plist form)
               (let ((c (getf plist :contacts)))
                 (unless (and (realp c) (plusp c)) (invalid "~S: число контактов ~S" form c))
                 (float c 0d0))))
        (dolist (form (read-forms synapses-path))
          (destructuring-bind (head a b &rest plist) form
            (declare (ignore head))
            (cond ((head-is form "SYNAPSE")
                   (push (list (neuron a form) (neuron b form) (contacts plist form)) chemical))
                  ((head-is form "GAP")
                   (let ((i (neuron a form)) (j (neuron b form)))
                     (when (= i j) (invalid "~S: щелевой контакт нейрона с самим собой" form))
                     (push (list (min i j) (max i j) (contacts plist form)) gap)))
                  ((head-is form "NMJ")
                   (let ((m (position (intern (symbol-name b) :keyword) muscles :key #'muscle-name)))
                     (unless m (invalid "~S: мышцы ~(~A~) нет в анатомии" form b))
                     (push (list (neuron a form) m (contacts plist form)) nmj)))
                  (t (invalid "неизвестная форма в файле синапсов: ~S" form))))))
      (make-connectome :neurons neurons
                       :chemical (coerce (nreverse chemical) 'simple-vector)
                       :gap (coerce (nreverse gap) 'simple-vector)
                       :nmj (coerce (nreverse nmj) 'simple-vector)
                       :source source))))

(defun write-connectome-files (neurons-path synapses-path connectome muscles &key (comment ""))
  "Записать CONNECTOME в два файла; MUSCLES — вектор мышц для имён nmj."
  (let ((neurons (connectome-neurons connectome)))
    (with-open-file (out neurons-path :direction :output :if-exists :supersede :external-format :utf-8)
      (with-standard-io-syntax
        (let ((*print-case* :downcase) (*print-readably* nil))
          (write-string comment out)
          (format out "(connectome-source~{ ~S ~S~})~%~%" (connectome-source connectome))
          (loop for n across neurons
                do (format out "(neuron ~(~A~) :class ~(~S~) :transmitter ~(~S~)~@[ :position ~,4F~] :nmj ~D)~%"
                           (neuron-name n) (neuron-class n) (neuron-transmitter n) (neuron-position n) (neuron-nmj n))))))
    (with-open-file (out synapses-path :direction :output :if-exists :supersede :external-format :utf-8)
      (with-standard-io-syntax
        (let ((*print-case* :downcase) (*print-readably* nil))
          (write-string comment out)
          (flet ((name (i) (neuron-name (aref neurons i)))
                 (num (c) (if (= c (round c)) (format nil "~D" (round c)) (format nil "~,2F" c))))
            (loop for (a b c) across (connectome-chemical connectome)
                  do (format out "(synapse ~(~A~) ~(~A~) :contacts ~A)~%" (name a) (name b) (num c)))
            (loop for (a b c) across (connectome-gap connectome)
                  do (format out "(gap ~(~A~) ~(~A~) :contacts ~A)~%" (name a) (name b) (num c)))
            (loop for (a m c) across (connectome-nmj connectome)
                  do (format out "(nmj ~(~A~) ~(~A~) :contacts ~A)~%" (name a) (muscle-name (aref muscles m)) (num c)))))))))

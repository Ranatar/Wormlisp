;;;; scripts/convert-connectome.lisp — anatomy/neurons.sexp и anatomy/synapses.sexp
;;;; из опубликованной таблицы коннектома.
;;;;
;;;; Запуск из корня репозитория:
;;;;   sbcl --non-interactive --no-userinit --load scripts/init.lisp --load scripts/convert-connectome.lisp
;;;;
;;;; Источник — таблица связей гермафродита Cook et al. 2019 (Nature 571:63),
;;;; в виде herm_full_edgelist.csv из репозитория openworm/c302 (MIT), коммит
;;;; f54d92d; копия лежит в anatomy/source/. Столбцы: Source, Target, Weight
;;;; (число контактов), Type (chemical или electrical).
;;;;
;;;; Какие нейроны берутся (docs/parameters.md, «Уровень 3»):
;;;;   - все нейроны с не меньше чем *nmj-threshold* нервно-мышечными
;;;;     контактами на мышцы стенки тела;
;;;;   - все мотонейроны вентрального тяжа (AS, DA, DB, DD, VA, VB, VC, VD) и
;;;;     хвоста (PDA, PDB), даже с меньшим числом контактов;
;;;;   - командные интернейроны AVA, AVB, AVD, AVE, PVC.
;;;; Связи — только между взятыми нейронами и на мышцы стенки тела. Щелевой
;;;; контакт таблица даёт в обе стороны; у 32 пар числа расходятся — берётся
;;;; среднее. Щелевые контакты мышц между собой в модель не входят: у мышц нет
;;;; потенциала.
;;;;
;;;; Медиатор: ГАМК — по McIntire et al. 1993 (DD, VD, RME, RIS, AVL, DVB);
;;;; остальные считаются возбуждающими, как у Kunert et al. 2014.

(asdf:load-system "worm/anatomy")

(in-package #:worm.anatomy)

(defparameter *nmj-threshold* 5)
(defparameter *motor-classes* '("AS" "DA" "DB" "DD" "VA" "VB" "VC" "VD" "PDA" "PDB"))
(defparameter *command-classes* '("AVA" "AVB" "AVD" "AVE" "PVC"))
(defparameter *gaba-classes* '("DD" "VD" "RME" "RIS" "AVL" "DVB"))
(defparameter *source* "anatomy/source/cook2019-herm-full-edgelist.csv")

(defun trim (s) (string-trim '(#\Space #\Tab #\Return) s))

(defun split-commas (line)
  (loop with start = 0
        for pos = (position #\, line :start start)
        collect (trim (subseq line start pos))
        while pos do (setf start (1+ pos))))

(defun neuron-class-of (name)
  "Класс: имя без номера у мотонейронов тяжа (VB06 → VB), без L/R у
двусторонних (AVBL → AVB), иначе само имя."
  (let ((stripped (string-right-trim "0123456789" name)))
    (cond ((member stripped *motor-classes* :test #'string=) stripped)
          ((and (> (length name) 3) (member (char name (1- (length name))) '(#\L #\R))
                (member (subseq name 0 (1- (length name))) *command-classes* :test #'string=))
           (subseq name 0 (1- (length name))))
          (t name))))

(defun class-starts-with (name classes)
  (some (lambda (c) (and (>= (length name) (length c)) (string= c name :end2 (length c)))) classes))

(defun bwm-muscle-name (cell)
  "dBWML7 → MDL07, vBWMR12 → MVR12; NIL для остальных клеток."
  (let ((prefix (and (>= (length cell) 5) (subseq cell 0 5))))
    (let ((q (cond ((string= prefix "dBWML") "MDL") ((string= prefix "dBWMR") "MDR")
                   ((string= prefix "vBWML") "MVL") ((string= prefix "vBWMR") "MVR"))))
      (and q (format nil "~A~2,'0D" q (parse-integer cell :start 5))))))

(let* ((root (worm.core:project-root))
       (source (merge-pathnames *source* root))
       (anatomy (load-anatomy :neurons-path #p"/nonexistent" :synapses-path #p"/nonexistent"))
       (muscles (anatomy-muscles anatomy))
       (rows (with-open-file (in source :external-format :utf-8)
               (read-line in)
               (loop for line = (read-line in nil) while line
                     for f = (split-commas line)
                     when (= (length f) 4)
                       collect (list (first f) (second f) (parse-integer (third f)) (fourth f)))))
       (nmj-total (make-hash-table :test 'equal)))
  ;; отбор нейронов
  (loop for (a b w type) in rows
        when (and (string= type "chemical") (bwm-muscle-name b))
          do (incf (gethash a nmj-total 0) w))
  (let* ((names (sort (remove-duplicates
                       (loop for (a b) in rows
                             append (loop for n in (list a b)
                                          when (and (not (bwm-muscle-name n))
                                                    (upper-case-p (char n 0))
                                                    (or (>= (gethash n nmj-total 0) *nmj-threshold*)
                                                        (member (neuron-class-of n) *motor-classes* :test #'string=)
                                                        (member (neuron-class-of n) *command-classes* :test #'string=)))
                                            collect n))
                       :test #'string=)
                      #'string<))
         (index (let ((h (make-hash-table :test 'equal))) (loop for n in names for i from 0 do (setf (gethash n h) i)) h))
         (muscle-index (lambda (cell) (let ((m (bwm-muscle-name cell)))
                                        (and m (position (intern m :keyword) muscles :key #'muscle-name)))))
         (nmj '()) (chemical '()) (gap (make-hash-table :test 'equal)))
    (loop for (a b w type) in rows
          for i = (gethash a index) for j = (gethash b index) for m = (funcall muscle-index b)
          do (cond ((and (string= type "chemical") i m) (push (list i m (float w 0d0)) nmj))
                   ((and (string= type "chemical") i j) (push (list i j (float w 0d0)) chemical))
                   ((and (string= type "electrical") i j (/= i j))
                    (push w (gethash (list (min i j) (max i j)) gap)))))
    (let* ((neurons
             (loop for n in names for i from 0
                   collect (let* ((targets (remove-if-not (lambda (e) (= (first e) i)) nmj))
                                  (total (reduce #'+ targets :key #'third))
                                  (pos (and (plusp total)
                                            (/ (reduce #'+ targets :key (lambda (e) (* (third e) (muscle-peak (aref muscles (second e))))))
                                               total))))
                             (make-neuron :name (intern (string-upcase n) :keyword)
                                          :class (intern (string-upcase (neuron-class-of n)) :keyword)
                                          :transmitter (if (class-starts-with n *gaba-classes*) :gaba :acetylcholine)
                                          :position pos :nmj (round total)))))
           (gaps (sort (loop for k being the hash-keys of gap using (hash-value ws)
                             collect (list (first k) (second k) (float (/ (reduce #'+ ws) (length ws)) 0d0)))
                       (lambda (x y) (or (< (first x) (first y)) (and (= (first x) (first y)) (< (second x) (second y)))))))
           (connectome (make-connectome
                        :neurons (coerce neurons 'simple-vector)
                        :chemical (coerce (sort (nreverse chemical) (lambda (x y) (or (< (first x) (first y)) (and (= (first x) (first y)) (< (second x) (second y)))))) 'simple-vector)
                        :gap (coerce gaps 'simple-vector)
                        :nmj (coerce (sort (nreverse nmj) (lambda (x y) (or (< (first x) (first y)) (and (= (first x) (first y)) (< (second x) (second y)))))) 'simple-vector)
                        :source (list :name "Cook et al. 2019, гермафродит, полная таблица связей"
                                      :file *source*
                                      :origin "github.com/openworm/c302 f54d92d9 c302/data/herm_full_edgelist.csv (MIT)"
                                      :md5 (worm.platform:md5-hex (uiop:read-file-string source))
                                      :selection (format nil "нервно-мышечных контактов >= ~D; мотонейроны ~{~A~^ ~}; командные ~{~A~^ ~}"
                                                         *nmj-threshold* *motor-classes* *command-classes*)))))
      (write-connectome-files (merge-pathnames "anatomy/neurons.sexp" root)
                              (merge-pathnames "anatomy/synapses.sexp" root)
                              connectome muscles
                              :comment ";;;; Порождён scripts/convert-connectome.lisp из anatomy/source/cook2019-herm-full-edgelist.csv.
;;;; Не править руками: правило отбора и формат — в скрипте и в worm.anatomy.

")
      (format t "~&Нейронов ~D, химических синапсов ~D, щелевых пар ~D, нервно-мышечных ~D~%"
              (length neurons) (length (connectome-chemical connectome)) (length gaps)
              (length (connectome-nmj connectome))))))

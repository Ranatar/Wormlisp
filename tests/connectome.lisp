;;;; tests/connectome.lisp — третий уровень: коннектом и градуальные нейроны (этап 12).
;;;;
;;;; Файлы коннектома читаются и сходятся с источником; сгенерированная
;;;; нервно-мышечная часть совпадает с интерпретатором, а её якобиан — с
;;;; конечными разностями (связь экземпляра нейронов с самим собой — через
;;;; щелевые контакты и синапсы); порог V_th — точка покоя сети; источник
;;;; коннектома попадает в meta.json прогона.

(in-package #:worm.tests)

(def-suite connectome :in worm :description "Уровень 3: коннектом, нейроны, связи.")
(in-suite connectome)

(defun connectome-of (&optional (anatomy (worm.anatomy:load-anatomy)))
  (worm.anatomy:anatomy-connectome anatomy))

(test connectome-files-load-and-match-the-source
  (let* ((c (connectome-of))
         (neurons (worm.anatomy:connectome-neurons c))
         (src (worm.anatomy:connectome-source c)))
    (is (= 147 (length neurons)))
    (is (string= (getf src :md5)
                 (worm.platform:md5-hex (uiop:read-file-string (merge-pathnames (getf src :file) (worm.core:project-root)))))
        "MD5 источника в файле нейронов совпадает с anatomy/source")
    (dolist (name '(:avbl :avbr :aval :avar :db01 :db07 :vb01 :vb11 :dd01 :dd06 :vd01 :vd13 :smddl :rmed))
      (is (worm.anatomy:neuron-index c name) "нейрон ~(~A~) есть" name))
    (is (eq :gaba (worm.anatomy:neuron-transmitter (aref neurons (worm.anatomy:neuron-index c :dd03)))))
    (is (eq :acetylcholine (worm.anatomy:neuron-transmitter (aref neurons (worm.anatomy:neuron-index c :db03)))))
    (is (every (lambda (g) (< (first g) (second g))) (worm.anatomy:connectome-gap c)) "одна запись на пару")
    (is (= 95 (length (remove-duplicates (map 'list #'second (worm.anatomy:connectome-nmj c)))))
        "все 95 мышц иннервированы")
    ;; положение мотонейронов растёт вдоль тяжа
    (let ((pos (loop for k from 1 to 11
                     collect (worm.anatomy:neuron-position
                              (aref neurons (worm.anatomy:neuron-index c (intern (format nil "VB~2,'0D" k) :keyword)))))))
      (is (< (first pos) (car (last pos))) "VB01 впереди VB11: ~S" pos))))

(defvar *connectome-models* (make-hash-table :test 'equal))

(defun connectome-model (&key (rings 10))
  "Модель третьего уровня на агаре, собранная один раз на число колец."
  (or (gethash rings *connectome-models*)
      (setf (gethash rings *connectome-models*)
            (worm.codegen:compile-model 'worm.components:worm-3d-connectome :params "agar-a-v1" :rings rings))))

(defun random-neural-state (gen seed)
  "Начальное состояние нервно-мышечной части с возмущением: потенциалы ±15 мВ,
активность синапсов 0…1, сигнал изгиба и кальций — малые."
  (let ((v (copy-seq (worm.codegen::model-layout-yn0 (worm.core:compiled-model-layout gen))))
        (state (sb-ext:seed-random-state seed)))
    (loop for (inst var off count) in (worm.codegen:model-neural-states gen)
          do (loop for i from off below (+ off count)
                   do (setf (aref v i)
                            (case var
                              (:v (+ (aref v i) (- (random 30d0 state) 15d0)))
                              (:s (random 1d0 state))
                              (:sense (- (random 0.4d0 state) 0.2d0))
                              (t (+ (aref v i) (- (random 0.2d0 state) 0.1d0)))))))
    v))

(test generated-connectome-matches-the-interpreter
  (let* ((gen (connectome-model))
         (model (worm.codegen:model-instance-of gen))
         (p (worm.codegen:model-param-vector gen))
         (y (perturbed (copy-seq (worm.codegen::model-layout-y0 (worm.core:compiled-model-layout gen))) 7 0.004d0))
         (aux (worm.core:make-dvec (length y)))
         (yn (random-neural-state gen 3))
         (obs (worm.core:make-dvec (worm.core:compiled-model-n-observables gen)))
         (clock (worm.core:dvec-from (list 0.2d0 1d-3 200)))
         (f (worm.core:make-dvec (length yn))))
    (is (null (worm.codegen:model-notes gen)))
    (funcall (worm.core:compiled-model-observables gen) y aux p obs)
    (funcall (worm.core:compiled-model-neural-rhs gen) yn obs (worm.core:make-dvec 0) p clock f)
    (let* ((states (loop for inst in (remove-duplicates (mapcar #'first (worm.codegen:model-neural-states gen)))
                         collect (cons inst (loop for (i var off count) in (worm.codegen:model-neural-states gen)
                                                  when (eq i inst) collect (cons var (subseq yn off (+ off count)))))))
           (r (worm.dsl:interpret model y :aux aux :time 0.2d0 :states states))
           (worst 0d0) (scale 0d0))
      (loop for (inst var off count) in (worm.codegen:model-neural-states gen)
            do (let ((expected (cdr (assoc var (cdr (assoc inst (worm.dsl:interpretation-derivs r)))))))
                 (dotimes (i count)
                   (setf scale (max scale (abs (aref expected i)))
                         worst (max worst (abs (- (aref expected i) (aref f (+ off i)))))))))
      (is (> scale 1d0))
      (is (< (/ worst scale) 1d-10) "правая часть против интерпретатора: ~,2E" (/ worst scale)))))

(test connectome-jacobian-matches-finite-differences
  (let* ((gen (connectome-model))
         (p (worm.codegen:model-param-vector gen))
         (y (perturbed (copy-seq (worm.codegen::model-layout-y0 (worm.core:compiled-model-layout gen))) 5 0.004d0))
         (aux (worm.core:make-dvec (length y)))
         (n (worm.core:compiled-model-n-neural gen))
         (yn (random-neural-state gen 11))
         (obs (worm.core:make-dvec (worm.core:compiled-model-n-observables gen)))
         (z (worm.core:make-dvec 0))
         (clock (worm.core:dvec-from (list 0.2d0 1d-3 200)))
         (jac (make-array (list n n) :element-type 'double-float :initial-element 0d0)))
    (funcall (worm.core:compiled-model-observables gen) y aux p obs)
    (funcall (worm.core:compiled-model-neural-jacobian gen) yn obs z p clock jac)
    (flet ((rhs (v)
             (let ((f (worm.core:make-dvec n)))
               (funcall (worm.core:compiled-model-neural-rhs gen) v obs z p clock f)
               f)))
      (let ((worst 0d0) (cross 0))
        (dotimes (k n)
          (let* ((eps (* 1d-6 (max 1d0 (abs (aref yn k)))))
                 (yp (copy-seq yn)) (ym (copy-seq yn)))
            (incf (aref yp k) eps) (decf (aref ym k) eps)
            (let ((fp (rhs yp)) (fm (rhs ym)))
              (dotimes (i n)
                (let ((fd (/ (- (aref fp i) (aref fm i)) (* 2 eps))))
                  (setf worst (max worst (/ (abs (- fd (aref jac i k))) (max 1d0 (abs fd)))))
                  (when (and (/= i k) (> (abs fd) 1d-6)) (incf cross)))))))
        (is (> cross 1000) "внедиагональных связей много: ~D" cross)
        (is (< worst 1d-5) "якобиан против разностей: ~,2E" worst)))))

(test neuron-threshold-is-the-network-rest-point
  ;; В покое сети V_eq = V_th + ln(φ_r/(1 − φ_r))/β, s = a_r·φ_r/(a_r·φ_r + a_d);
  ;; без тока команды и рецептора сеть стоит: dV/dt = 0 и ds/dt = 0.
  (let* ((set (worm.params:derive-param-set (worm.params:load-param-set "agar-a-v1") "rest"
                                            '((:command-current 0d0 nil))))
         (gen (connectome-model))
         (p (worm.codegen:model-param-vector gen set))
         (phi (worm.params:param-value set :neuron-rest-activity))
         (beta (worm.params:param-value set :syn-beta))
         (ar (worm.params:param-value set :syn-rise)) (ad (worm.params:param-value set :syn-decay))
         (y (copy-seq (worm.codegen::model-layout-y0 (worm.core:compiled-model-layout gen))))
         (aux (worm.core:make-dvec (length y)))
         (yn (copy-seq (worm.codegen::model-layout-yn0 (worm.core:compiled-model-layout gen))))
         (obs (worm.core:make-dvec (worm.core:compiled-model-n-observables gen)))
         (clock (worm.core:dvec-from (list 0d0 1d-3 0)))
         (f (worm.core:make-dvec (length yn)))
         (threshold (cdr (assoc :neuron-threshold
                                (worm.dsl::running-arrays (worm.dsl::running-named (worm.codegen:model-instance-of gen) :neurons)))))
         (s-eq (/ (* ar phi) (+ (* ar phi) ad)))
         (shift (/ (log (/ phi (- 1 phi))) beta)))
    (loop for (inst var off count) in (worm.codegen:model-neural-states gen)
          when (eq inst :neurons)
            do (dotimes (i count)
                 (setf (aref yn (+ off i)) (case var (:v (+ (aref threshold i) shift)) (:s s-eq) (t 0d0)))))
    (funcall (worm.core:compiled-model-observables gen) y aux p obs)
    (funcall (worm.core:compiled-model-neural-rhs gen) yn obs (worm.core:make-dvec 0) p clock f)
    (let ((worst 0d0))
      (loop for (inst var off count) in (worm.codegen:model-neural-states gen)
            when (and (eq inst :neurons) (member var '(:v :s)))
              do (dotimes (i count) (setf worst (max worst (abs (aref f (+ off i)))))))
      (is (< worst 1d-6) "сеть в покое: ~,2E" worst))
    (is (< (reduce #'min threshold) (reduce #'max threshold)) "пороги различны")))

(test run-meta-records-the-connectome-source
  (let* ((gen (connectome-model :rings 8))
         (run (worm.runtime:start-run gen :params "agar-a-v1" :dt 1d-3 :paused t :directory (runtime-dir "connectome-meta"))))
    (unwind-protect
         (progn
           (worm.runtime:step-run 20 run)
           (is (eq :paused (worm.runtime:run-status run))))
      (worm.runtime:stop-run run))
    (let* ((meta (worm.io:read-json-file (merge-pathnames "meta.json" (worm.runtime:run-directory run))))
           (c (gethash "connectome" meta)))
      (is (hash-table-p c))
      (is (string= (getf (worm.anatomy:connectome-source (connectome-of)) :md5) (gethash "md5" c)))
      (is (= 147 (gethash "neurons" c))))))

(test numeric-sweep-keys-change-the-job-not-the-parameters
  ;; :vary по dt и neural-every меняет шаг и R задания, а не набор параметров
  (let ((path (merge-pathnames "cache/test-batch/numeric.sexp" (worm.core:project-root))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "(sweep numeric :base #p\"tests/data/batch/tiny.sexp\"~%  :vary ((neural-every :list (5 10)) (dt :list (1d-3 5d-4))))~%"))
    (let ((jobs (worm.batch:expand-sweep (worm.batch:load-sweep path))))
      (is (= 4 (length jobs)))
      (is (equal '(5 5 10 10) (mapcar (lambda (j) (getf j :neural-every)) jobs)))
      (is (equal '(1d-3 5d-4 1d-3 5d-4) (mapcar (lambda (j) (getf j :dt)) jobs)))
      (is (notany (lambda (j) (or (assoc :dt (getf j :set)) (assoc :neural-every (getf j :set)))) jobs))
      (is (= 4 (length (remove-duplicates (mapcar (lambda (j) (getf j :id)) jobs) :test #'string=)))))))

(test neuron-receptive-fields-and-head-sides
  ;; Поле B-нейрона: назад от его положения с положительным весом, при
  ;; rf-direction :anterior — впереди него с отрицательным; у остальных
  ;; нейронов поля нет. Сторона головных нейронов — по их мышцам.
  (flet ((fields (direction)
           (let* ((set (worm.params:derive-param-set (worm.params:load-param-set "agar-a-v1") "rf"
                                                     (list (list :rf-direction direction nil))))
                  (model (worm.dsl:instantiate (worm.dsl:find-model 'worm.components:worm-3d-connectome)
                                               :params set :rings 10))
                  (neurons (worm.dsl::running-named model :neurons))
                  (edges (worm.dsl::running-named model :receptors))
                  (csr (cdr (find-if (lambda (rel) (eq (worm.dsl::wire-via (car rel)) :receptive-field))
                                     (worm.dsl::model-instance-relations model)))))
             (values csr (worm.dsl::running-elements neurons) (worm.dsl::running-elements edges)))))
    (multiple-value-bind (csr el edge-el) (fields :posterior)
      (let ((prop (cdr (assoc :proprioceptive el))) (pos (cdr (assoc :position el)))
            (epos (cdr (assoc :position edge-el))))
        (dotimes (u (length prop))
          (if (plusp (aref prop u))
              (is (and (aref csr u) (every (lambda (e) (and (plusp (cdr e)) (>= (aref epos (car e)) (aref pos u)))) (aref csr u))))
              (is (null (aref csr u)))))
        (let ((side (cdr (assoc :head-side el))) (cord (cdr (assoc :cord el))))
          (is (some #'plusp side)) (is (some #'minusp side))
          (is (every (lambda (s c) (or (zerop c) (zerop s))) side cord) "у мотонейронов тяжа стороны головы нет"))))
    (multiple-value-bind (csr el edge-el) (fields :anterior)
      (let ((prop (cdr (assoc :proprioceptive el))) (pos (cdr (assoc :position el)))
            (epos (cdr (assoc :position edge-el))))
        (dotimes (u (length prop))
          (when (and (plusp (aref prop u)) (aref csr u))
            (is (every (lambda (e) (and (minusp (cdr e)) (<= (aref epos (car e)) (aref pos u)))) (aref csr u)))))))))

;;; Походка третьего уровня (make test-full)

(in-suite physics)

(test connectome-network-crawls-forward-on-agar
  ;; Сеть коннектома без внешнего ритма: бегущая от головы к хвосту волна и
  ;; ползание вперёд на агаре (этап 12; дизайн, «Главный тест»).
  (let* ((dir (runtime-dir "connectome-agar"))
         (model (worm.codegen:compile-model 'worm.components:worm-3d-connectome :params "agar-a-v1" :cache t))
         (run (worm.runtime:start-run model :params "agar-a-v1" :dt 1d-3 :paused t :record t :frames-every 0.01d0
                                            :directory dir :policy :batch)))
    (worm.runtime:step-run 8000 run)
    (worm.runtime:stop-run run)
    (is (eq :done (worm.runtime:run-status run)))
    (let* ((r (worm.analysis:load-run dir))
           (m (worm.analysis:gait-metrics r :from (position-if (lambda (x) (>= x 4d0)) (worm.analysis:run-data-times r)))))
      (is (< 0.3d0 (or (getf m :frequency) 0) 0.9d0) "частота ~A Гц" (getf m :frequency))
      (is (< 0.5d0 (or (getf m :wavelength) 0) 1.4d0) "длина волны ~A L" (getf m :wavelength))
      (is (> (getf m :speed) 0.04d0) "ползёт вперёд: ~,4F мм/с" (getf m :speed)))))

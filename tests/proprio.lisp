;;;; tests/proprio.lisp — проприоцептивная цепь уровня 2 (этап 10).
;;;;
;;;; Язык модели: дискретное состояние и события, семейство :prop-unit,
;;;; отношения :receptive-field и :innervation. Сгенерированные
;;;; neural-discrete и neural-output совпадают с интерпретатором; события
;;;; дают гистерезис B-нейрона. Физика походки — tests/proprio-physics.lisp.

(in-package #:worm.tests)

(def-suite proprio :in worm :description "Уровень 2: B-единицы, события, рецептивные поля.")
(in-suite proprio)

(defun proprio-instance (&key (params "agar-a-v1") (rings 10))
  (worm.dsl:instantiate (worm.dsl:find-model 'worm.components:worm-3d-proprio) :params params :rings rings))

(defun relation-of (model dst-instance port)
  (cdr (find-if (lambda (pair) (let ((w (car pair)))
                                 (and (eq (worm.dsl::wire-dst-instance w) dst-instance)
                                      (eq (worm.dsl::wire-dst-port w) port))))
                (worm.dsl::model-instance-relations model))))

(test units-and-receptive-fields
  (let* ((model (proprio-instance))
         (units (worm.dsl::running-named model :units))
         (csr (relation-of model :units :bend))
         (receptors (worm.dsl::running-named model :receptors))
         (pos (cdr (assoc :position (worm.dsl::running-elements receptors)))))
    (is (= 12 (worm.dsl::running-count units)))
    (is (equal '(:db :vb) (mapcar #'car (worm.dsl::running-discrete units))))
    ;; поле назад: от начала участка единицы на rf-length = 0,35 L, сумма
    ;; весов — 2·N_SR, N_SR = 48·0,35
    (let ((first-unit (aref csr 0)) (last-unit (aref csr 11)) (n2 (* 2 48 0.35d0)))
      (is (every (lambda (pair) (<= 0 (aref pos (car pair)) 0.35d0)) first-unit))
      (is (worm.core:approx= n2 (reduce #'+ first-unit :key #'cdr) :rel 1d-12) "вес поля = 2·N_SR")
      (is (every (lambda (pair) (>= (aref pos (car pair)) (/ 11 12))) last-unit))
      (is (worm.core:approx= n2 (reduce #'+ last-unit :key #'cdr) :rel 1d-12) "укороченное поле нормировано"))
    ;; дорсальных и вентральных рёбер в поле поровну
    (let ((side (cdr (assoc :side (worm.dsl::running-elements receptors)))))
      (is (zerop (loop for (e) in (aref csr 3) sum (aref side e)))))))

(test receptive-field-direction-is-a-parameter
  (let* ((path (merge-pathnames "cache/test-proprio/params/forward-agar.sexp" (worm.core:project-root))))
    (ensure-directories-exist path)
    (dolist (f (directory (merge-pathnames "params/*.sexp" (worm.core:project-root))))
      (uiop:copy-file f (merge-pathnames (file-namestring f) path)))
    (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
      (format out "(params forward-agar :extends agar-a-v1~%  (rf-direction :anterior nil)~%  (rf-length 0.2 nil))~%"))
    (let* ((model (proprio-instance :params (worm.params:load-param-set path)))
           (csr (relation-of model :units :bend))
           (pos (cdr (assoc :position (worm.dsl::running-elements (worm.dsl::running-named model :receptors))))))
      ;; единица 6 (с 5/12 по 6/12): поле — 0,2 L впереди начала её участка
      (is (every (lambda (pair) (<= (- 5/12 0.2d0) (aref pos (car pair)) 5/12)) (aref csr 5)))
      ;; первая единица — головной осциллятор с полем назад
      (is (every (lambda (pair) (<= 0 (aref pos (car pair)) 0.2d0)) (aref csr 0)))
      (is (plusp (reduce #'+ (aref csr 0) :key #'cdr)))
      (is (worm.core:approx= (* -2 48 0.2d0) (reduce #'+ (aref csr 5) :key #'cdr) :rel 1d-12)
          "поле вперёд повторяет изгиб: знак сигнала обратный"))))

(test innervation-follows-muscle-position-and-side
  (let* ((model (proprio-instance))
         (csr (relation-of model :muscles :drive))
         (muscles (worm.dsl::running-named model :muscles))
         (pos (cdr (assoc :position (worm.dsl::running-elements muscles))))
         (side (cdr (assoc :side (worm.dsl::running-elements muscles)))))
    (dotimes (m 95)
      (destructuring-bind ((unit . w)) (aref csr m)
        (is (= unit (min 11 (floor (* 12 (aref pos m))))))
        (is (= w (aref side m)))))))

(defun units-interpretation (model y bend-scale &key db vb)
  "Интерпретатор с событиями: дискретное состояние до — DB, VB (векторы)."
  (let ((units (worm.dsl::running-named model :units)))
    (when db (replace (cdr (assoc :db (worm.dsl::running-discrete units))) db))
    (when vb (replace (cdr (assoc :vb (worm.dsl::running-discrete units))) vb))
    (let ((p (worm.dsl::running-params units)))
      (setf (cdr (assoc :sr-scale p)) bend-scale))
    (worm.dsl:interpret model y :events t)))

(test b-neurons-switch-with-hysteresis
  ;; Прямое тело: изгиба нет, вход DB = 0,675 — между порогами 0,25 и 0,75,
  ;; поэтому DB сохраняет прежнее состояние; вход VB = 1,175 − db.
  (let* ((model (proprio-instance))
         (y (worm.topology:tube-rest-coords (worm.dsl::model-instance-topology model))))
    (flet ((states (db vb)
             (let* ((r (units-interpretation model y 0.25d0 :db db :vb vb))
                    (d (cdr (assoc :units (worm.dsl:interpretation-discrete r)))))
               (list (cdr (assoc :db d)) (cdr (assoc :vb d))))))
      (let ((zeros (worm.core:make-dvec 12)) (ones (make-array 12 :element-type 'double-float :initial-element 1d0)))
        (destructuring-bind (db vb) (states zeros zeros)
          (is (every #'zerop db) "DB остаётся выключенным")
          (is (every (lambda (x) (= x 1d0)) vb) "VB включается: 1,175 > 0,75"))
        (destructuring-bind (db vb) (states ones zeros)
          (is (every (lambda (x) (= x 1d0)) db) "DB остаётся включённым")
          (is (every #'zerop vb) "VD тормозит VB: 0,175 < 0,25"))))))

(test generated-discrete-step-matches-the-interpreter
  (let* ((gen (worm.codegen:compile-model 'worm.components:worm-3d-proprio :params "agar-a-v1" :rings 10))
         (model (worm.codegen:model-instance-of gen))
         (p (worm.codegen:model-param-vector gen))
         (y (let ((v (copy-seq (worm.codegen::model-layout-y0 (worm.core:compiled-model-layout gen)))))
              ;; изгиб: дуга кривизны 3 1/мм по оси x в плоскости дорсо-вентральной оси
              (perturbed v 5 0.004d0)))
         (aux (worm.core:make-dvec (length y)))
         (z (let ((v (worm.core:make-dvec 24) ) (state (sb-ext:seed-random-state 3)))
              (dotimes (i 24 v) (setf (aref v i) (float (random 2 state) 0d0)))))
         (obs (worm.core:make-dvec (worm.core:compiled-model-n-observables gen)))
         (clock (worm.core:dvec-from (list 0.2d0 1d-3 200))))
    (is (= 24 (worm.core:compiled-model-n-discrete gen)))
    (is (null (worm.codegen:model-notes gen)))
    (funcall (worm.core:compiled-model-observables gen) y aux p obs)
    (let ((z-gen (copy-seq z)))
      (funcall (worm.core:compiled-model-neural-discrete gen)
               (worm.codegen::model-layout-yn0 (worm.core:compiled-model-layout gen)) obs z-gen p clock)
      (let* ((r (units-interpretation model y (aref p (position :sr-scale (worm.codegen:model-param-names gen)))
                                      :db (subseq z 0 12) :vb (subseq z 12 24)))
             (d (cdr (assoc :units (worm.dsl:interpretation-discrete r)))))
        (is (equalp z-gen (concatenate 'worm.core:dvec (cdr (assoc :db d)) (cdr (assoc :vb d))))
            "события: ~S против ~S" z-gen (concatenate 'worm.core:dvec (cdr (assoc :db d)) (cdr (assoc :vb d))))
        ;; натяжение мышц от того же дискретного состояния
        (let ((syn (worm.core:make-dvec (worm.core:compiled-model-n-syn gen)))
              (yn (worm.codegen::model-layout-yn0 (worm.core:compiled-model-layout gen))))
          (funcall (worm.core:compiled-model-neural-output gen) yn obs z-gen p clock syn)
          (let ((motor (cdr (assoc :motor (cdr (assoc :units (worm.dsl:interpretation-outputs r)))))))
            (is (equalp motor (map 'worm.core:dvec (lambda (d v) (- d v)) (subseq z-gen 0 12) (subseq z-gen 12 24))))))))))

(test proprio-run-keeps-discrete-state-in-checkpoints
  (let* ((gen (worm.codegen:compile-model 'worm.components:worm-3d-proprio :params "agar-a-v1" :rings 10))
         (clean (worm.runtime:make-model-integrator gen :h 1d-3 :checkpoint-every 100))
         (interrupted (worm.runtime:make-model-integrator gen :h 1d-3 :checkpoint-every 100)))
    (worm.solver:run-steps clean 300)
    (worm.solver:run-steps interrupted 250)
    (worm.solver:restore-checkpoint interrupted)
    (worm.solver:run-steps interrupted 100)
    (is (equalp (worm.solver:integrator-z clean) (worm.solver:integrator-z interrupted)))
    (is (equalp (worm.solver:integrator-y-mech clean) (worm.solver:integrator-y-mech interrupted)))
    (is (some #'plusp (worm.solver:integrator-z clean)) "B-нейроны включаются")))

;;; Проводимостная версия (этап 11): выход единицы зависит от её состояния,
;;; и якобиан нервно-мышечной части собирается через связи.

(test coupled-neural-jacobian-matches-finite-differences
  (let* ((gen (worm.codegen:compile-model 'worm.components:worm-3d-proprio-graded :params "agar-a-v1" :rings 10))
         (p (worm.codegen:model-param-vector gen))
         (y (perturbed (copy-seq (worm.codegen::model-layout-y0 (worm.core:compiled-model-layout gen))) 5 0.004d0))
         (aux (worm.core:make-dvec (length y)))
         (n (worm.core:compiled-model-n-neural gen))
         (yn (let ((v (copy-seq (worm.codegen::model-layout-yn0 (worm.core:compiled-model-layout gen))))
                   (state (sb-ext:seed-random-state 11)))
               ;; потенциалы вокруг порогов: ±20 мВ от покоя, сигнал и кальций — малые
               (dotimes (i n v) (incf (aref v i) (- (random 0.4d0 state) 0.2d0)))
               (loop for (inst var off count) in (worm.codegen:model-neural-states gen)
                     when (member var '(:ud :uv))
                       do (loop for i from off below (+ off count)
                                do (setf (aref v i) (- (random 40d0 state) 10d0))))
               v))
         (obs (worm.core:make-dvec (worm.core:compiled-model-n-observables gen)))
         (z (worm.core:make-dvec 0))
         (clock (worm.core:dvec-from (list 0.2d0 1d-3 200)))
         (jac (make-array (list n n) :element-type 'double-float :initial-element 0d0)))
    (is (null (worm.codegen:model-notes gen)))
    (is (= 0 (worm.core:compiled-model-n-discrete gen)) "у проводимостной версии нет событий")
    (funcall (worm.core:compiled-model-observables gen) y aux p obs)
    (funcall (worm.core:compiled-model-neural-jacobian gen) yn obs z p clock jac)
    (flet ((rhs (v)
             (let ((f (worm.core:make-dvec n)))
               (funcall (worm.core:compiled-model-neural-rhs gen) v obs z p clock f)
               f)))
      (let ((worst 0d0) (cross 0d0))
        (dotimes (k n)
          (let* ((eps (* 1d-6 (max 1d0 (abs (aref yn k)))))
                 (yp (copy-seq yn)) (ym (copy-seq yn)))
            (incf (aref yp k) eps) (decf (aref ym k) eps)
            (let ((fp (rhs yp)) (fm (rhs ym)))
              (dotimes (i n)
                (let ((fd (/ (- (aref fp i) (aref fm i)) (* 2 eps))))
                  (setf worst (max worst (/ (abs (- fd (aref jac i k))) (max 1d0 (abs fd)))))
                  (when (and (/= i k) (> (abs fd) 1d-3)) (setf cross (max cross (abs fd)))))))))
        (is (> cross 1d0) "связи между экземплярами есть: ~,2E" cross)
        (is (< worst 1d-5) "якобиан против разностей: ~,2E" worst)))))

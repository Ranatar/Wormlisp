;;;; tests/codegen.lisp — кодогенерация (этап 7).
;;;;
;;;; Условия готовности: сгенерированные ядра, интерпретатор и ручные ядра
;;;; совпадают до округления на случайных состояниях; глобальный якобиан
;;;; совпадает с конечными разностями, ширина ленты равна фактической; в
;;;; ядрах нет заметок о боксировании, шаг не выделяет памяти.
;;;;
;;;; Модели worm-3d (мышцы) и worm-3d-kinematic (натяжение этапа 3) на
;;;; языке модели сверяются с ручной моделью tests/oracle/ тех же наборов
;;;; параметров — функция за функцией.

(in-package #:worm.tests)

(def-suite codegen :in worm :description "worm.codegen и модели worm.components.")
(in-suite codegen)

(defvar *generated* (make-hash-table :test 'equal))

(defun generated (model params &key (rings 10))
  (let ((key (list model params rings)))
    (or (gethash key *generated*)
        (setf (gethash key *generated*)
              (worm.codegen:compile-model model :params params :rings rings)))))

(defun oracle-like (params &key (rings 10) muscles)
  "Ручная модель с теми же параметрами, что модель на языке модели."
  (let ((set (worm.params:load-param-set params)))
    (worm.oracle:build-body :params set :rings rings :muscles muscles
                            :amplitude (if muscles
                                           (worm.params:param-value set :gen-amplitude)
                                           (worm.params:param-value set :tension-amplitude))
                            :frequency (worm.params:param-value set :gen-frequency)
                            :wavelength (worm.params:param-value set :gen-wavelength)
                            :ramp (worm.params:param-value set :gen-ramp))))

(defstruct probe y aux yn time)

(defun random-probe (y0 n-neural seed &key (n-muscles 95))
  (let ((state (sb-ext:seed-random-state seed)))
    (flet ((rnd (scale) (* scale (- (random 2d0 state) 1d0))))
      (make-probe :y (map 'worm.core:dvec (lambda (x) (+ x (rnd 0.002d0))) y0)
                  :aux (map 'worm.core:dvec (lambda (x) (declare (ignore x)) (rnd 0.3d0)) y0)
                  :yn (let ((v (worm.core:make-dvec n-neural)))
                        (dotimes (i (min n-muscles n-neural)) (setf (aref v i) (random 1d0 state)))
                        (loop for i from n-muscles below n-neural do (setf (aref v i) (rnd 2d0)))
                        v)
                  :time 0.37d0))))

(defun max-diff (a b)
  (loop for i below (length a) maximize (abs (- (aref a i) (aref b i)))))

(defun max-abs (a) (loop for x across a maximize (abs x)))

(defun syn-from-muscle-tension (tension binding n-long)
  "Натяжение по продольным рёбрам из натяжений клеток: Σ w·T."
  (let ((out (worm.core:make-dvec n-long))
        (offsets (worm.anatomy:muscle-binding-offsets binding))
        (edges (worm.anatomy:muscle-binding-edges binding))
        (weights (worm.anatomy:muscle-binding-weights binding)))
    (dotimes (m (length tension) out)
      (loop for q from (aref offsets m) below (aref offsets (1+ m))
            do (incf (aref out (aref edges q)) (* (aref weights q) (aref tension m)))))))

(defun compare-with-oracle (params &key muscles (seed 1) (rings 10) probe)
  "Расхождения функций сгенерированной модели и ручной: plist. PROBE —
состояние (по умолчанию случайное возмущение начального)."
  (let* ((gen (generated (if muscles 'worm.components:worm-3d 'worm.components:worm-3d-kinematic) params :rings rings))
         (body (oracle-like params :rings rings :muscles muscles))
         (oracle (worm.oracle:body-model body))
         (p (worm.codegen:model-param-vector gen))
         (op (worm.oracle:body-pvec body))
         (probe (or probe (random-probe (worm.oracle:body-y0 body) (worm.core:compiled-model-n-neural oracle) seed)))
         (y (probe-y probe)) (aux (probe-aux probe)) (yn (probe-yn probe))
         (clock (worm.core:dvec-from (list (probe-time probe) 2d-4 7)))
         (n (length y))
         (results '()))
    (is (= n (worm.core:compiled-model-n-mech gen)))
    ;; наблюдаемые и нервно-мышечная часть
    (let ((oobs (worm.core:make-dvec (worm.core:compiled-model-n-observables oracle)))
          (gobs (worm.core:make-dvec (worm.core:compiled-model-n-observables gen))))
      (funcall (worm.core:compiled-model-observables oracle) y aux op oobs)
      (when (worm.core:compiled-model-observables gen)
        (funcall (worm.core:compiled-model-observables gen) y aux p gobs))
      (when muscles
        (let* ((blocks (worm.codegen::model-layout-obs-blocks (worm.core:compiled-model-layout gen)))
               (block (lambda (port) (third (find port blocks :key #'second))))
               (len (funcall block :attached-length)) (rest (funcall block :attached-rest))
               (rate (funcall block :attached-rate))
               (worst-l 0d0) (worst-r 0d0))
          (dotimes (m 95)
            (setf worst-l (max worst-l (abs (- (/ (aref gobs (+ len m)) (aref gobs (+ rest m))) (aref oobs (+ rings m)))))
                  worst-r (max worst-r (abs (- (/ (aref gobs (+ rate m)) (aref gobs (+ rest m))) (aref oobs (+ rings 95 m)))))))
          (push (list :lambda worst-l :rate worst-r) results)
          ;; правая часть и якобиан
          (let ((of (worm.core:make-dvec (length yn))) (gf (worm.core:make-dvec (length yn)))
                (oj (worm.core:make-dmat (length yn) (length yn))) (gj (worm.core:make-dmat (length yn) (length yn))))
            (funcall (worm.core:compiled-model-neural-rhs oracle) yn oobs (worm.core:make-dvec 0) op clock of)
            (funcall (worm.core:compiled-model-neural-rhs gen) yn gobs (worm.core:make-dvec 0) p clock gf)
            (funcall (worm.core:compiled-model-neural-jacobian oracle) yn oobs (worm.core:make-dvec 0) op clock oj)
            (funcall (worm.core:compiled-model-neural-jacobian gen) yn gobs (worm.core:make-dvec 0) p clock gj)
            (push (list :rhs (/ (max-diff of gf) (max-abs of))
                        :neural-jacobian (/ (max-diff (sb-ext:array-storage-vector oj) (sb-ext:array-storage-vector gj))
                                            (max-abs (sb-ext:array-storage-vector oj))))
                  results))))
      ;; натяжение мышц и силы
      (let* ((osyn (worm.core:make-dvec 95))
             (gsyn (worm.core:make-dvec (worm.core:compiled-model-n-syn gen)))
             (og (worm.core:make-dvec n)) (gg (worm.core:make-dvec n)))
        (funcall (worm.core:compiled-model-neural-output oracle) yn oobs (worm.core:make-dvec 0) op clock osyn)
        (funcall (worm.core:compiled-model-neural-output gen) yn gobs (worm.core:make-dvec 0) p clock gsyn)
        (let ((expected (syn-from-muscle-tension osyn (worm.oracle:body-binding body) (length gsyn))))
          (push (list :tension (/ (max-diff expected gsyn) (max (max-abs expected) 1d-12))) results))
        (funcall (worm.core:compiled-model-mech-forces oracle) y osyn op og)
        (funcall (worm.core:compiled-model-mech-forces gen) y gsyn p gg)
        (push (list :forces (/ (max-diff og gg) (max-abs og)) :force-scale (max-abs og)) results)
        ;; матрица шага
        (let* ((okl (worm.core:compiled-model-lower-band oracle))
               (gkl (worm.core:compiled-model-lower-band gen)) (gku (worm.core:compiled-model-upper-band gen))
               (oab (worm.core:make-band n okl okl)) (gab (worm.core:make-band n gkl gku)))
          (funcall (worm.core:compiled-model-mech-matrix oracle) y osyn op clock oab)
          (funcall (worm.core:compiled-model-mech-matrix gen) y gsyn p clock gab)
          (let ((od (band-to-dense oab n okl okl)) (gd (band-to-dense gab n gkl gku)))
            (push (list :matrix (/ (max-diff (sb-ext:array-storage-vector od) (sb-ext:array-storage-vector gd))
                                   (max-abs (sb-ext:array-storage-vector od)))
                        :band (list gkl gku okl))
                  results)))))
    (apply #'append (nreverse results))))

(test generated-model-matches-the-oracle
  (loop for (params muscles) in '(("water-v1" t) ("agar-a-v1" t) ("water-v1" nil) ("agar-a-v1" nil))
        do (let ((r (compare-with-oracle params :muscles muscles)))
             (dolist (key '(:lambda :rate :rhs :neural-jacobian :tension :forces :matrix))
               (when (getf r key)
                 (is (< (getf r key) 1d-10) "~A, ~:[натяжение~;мышцы~]: ~(~A~) расходится на ~,2E"
                     params muscles key (getf r key))))
             (is (> (getf r :force-scale) 1d0) "силы заметные: ~,2F мкН" (getf r :force-scale))
             (is (equal '(47 47 47) (getf r :band)) "лента ~S" (getf r :band)))))

(test generated-model-matches-the-interpreter
  ;; Тот же model-spec, исполненный интерпретатором IR, — силы при одном
  ;; и том же состоянии нервно-мышечной части.
  (let* ((gen (generated 'worm.components:worm-3d "agar-a-v1"))
         (model (worm.codegen:model-instance-of gen))
         (p (worm.codegen:model-param-vector gen))
         (probe (random-probe (worm.codegen::model-layout-y0 (worm.core:compiled-model-layout gen))
                              (worm.core:compiled-model-n-neural gen) 9))
         (y (probe-y probe)) (aux (probe-aux probe)) (yn (probe-yn probe))
         (time (probe-time probe))
         (clock (worm.core:dvec-from (list time 0 0)))
         (obs (worm.core:make-dvec (worm.core:compiled-model-n-observables gen)))
         (syn (worm.core:make-dvec (worm.core:compiled-model-n-syn gen)))
         (g (worm.core:make-dvec (length y)))
         (f (worm.core:make-dvec (length yn))))
    (funcall (worm.core:compiled-model-observables gen) y aux p obs)
    (funcall (worm.core:compiled-model-neural-output gen) yn obs (worm.core:make-dvec 0) p clock syn)
    (funcall (worm.core:compiled-model-mech-forces gen) y syn p g)
    (funcall (worm.core:compiled-model-neural-rhs gen) yn obs (worm.core:make-dvec 0) p clock f)
    (let ((result (worm.dsl:interpret model y :aux aux :time time
                                              :states `((:muscles (:ca . ,(subseq yn 0 95)) (:rate . ,(subseq yn 95)))))))
      (is (< (/ (max-diff g (worm.dsl:interpretation-forces result)) (max-abs g)) 1d-12) "силы")
      (let ((derivs (cdr (assoc :muscles (worm.dsl:interpretation-derivs result)))))
        (is (< (max-diff f (concatenate 'worm.core:dvec (cdr (assoc :ca derivs)) (cdr (assoc :rate derivs)))) 1d-10)
            "производные состояния")))
    ;; одно ядро на одном элементе
    (let ((values (worm.codegen:interpret-kernel gen 'cuticle-long 3 y)))
      (is (= 9 (length values)) "три выхода и шесть сил ребра"))))

(test global-jacobian-matches-finite-differences
  ;; J из матрицы шага: A(h) = B − h·J, J = (A(h₁) − A(h₂))/(h₂ − h₁); против
  ;; центральных разностей сгенерированных сил. На агаре — все ядра, кроме
  ;; разрыва контакта: узлы держатся над поверхностью или под ней.
  (let* ((gen (generated 'worm.components:worm-3d "agar-a-v1" :rings 6))
         (p (worm.codegen:model-param-vector gen))
         (y (let ((v (copy-seq (worm.codegen::model-layout-y0 (worm.core:compiled-model-layout gen)))))
              (perturbed v 13 0.003d0)))
         (n (length y))
         (kl (worm.core:compiled-model-lower-band gen)) (ku (worm.core:compiled-model-upper-band gen))
         (syn (let ((v (worm.core:make-dvec (worm.core:compiled-model-n-syn gen))))
                (dotimes (i (length v) v) (setf (aref v i) (* 3d0 (sin i))))))
         (eps 1d-7))
    ;; контакт: держать узлы подальше от z = 0
    (loop for i from 2 below n by 3 do (when (< (abs (aref y i)) 1d-4) (decf (aref y i) 3d-4)))
    (flet ((matrix (h)
             (let ((ab (worm.core:make-band n kl ku)))
               (funcall (worm.core:compiled-model-mech-matrix gen) y syn p (worm.core:dvec-from (list 0 h 0)) ab)
               (band-to-dense ab n kl ku)))
           (forces (yy) (let ((g (worm.core:make-dvec n)))
                          (funcall (worm.core:compiled-model-mech-forces gen) yy syn p g) g)))
      (let ((a1 (matrix 1d-3)) (a2 (matrix 2d-3)) (worst 0d0) (scale 0d0) (reach 0))
        (dotimes (k n)
          (let ((yp (copy-seq y)) (ym (copy-seq y)))
            (incf (aref yp k) eps) (decf (aref ym k) eps)
            (let ((gp (forces yp)) (gm (forces ym)))
              (dotimes (i n)
                (let ((fd (/ (- (aref gp i) (aref gm i)) (* 2 eps)))
                      (an (/ (- (aref a1 i k) (aref a2 i k)) 1d-3)))
                  (setf scale (max scale (abs fd)) worst (max worst (abs (- fd an))))
                  (unless (zerop (aref a1 i k)) (setf reach (max reach (abs (- i k))))))))))
        (is (< (/ worst scale) 1d-6) "якобиан: относительное расхождение ~,2E" (/ worst scale))
        (is (= reach kl ku) "ширина ленты ~D/~D равна фактической ~D" kl ku reach)))))

(test generated-code-has-no-boxing-and-steps-allocate-nothing
  (let ((gen (generated 'worm.components:worm-3d "agar-a-v1")))
    (is (null (worm.codegen:model-notes gen)) "заметки оптимизатора: ~S" (worm.codegen:model-notes gen))
    (let ((it (worm.solver:make-integrator gen :params (worm.codegen:model-param-vector gen)
                                               :y-mech (worm.codegen::model-layout-y0 (worm.core:compiled-model-layout gen))
                                               :y-neural (worm.codegen::model-layout-yn0 (worm.core:compiled-model-layout gen))
                                               :h 1d-3 :neural-every 10 :checkpoint-every 100)))
      (worm.solver:run-steps it 200)
      (let ((bytes (bytes-consed-by (lambda () (loop repeat 3000 do (worm.solver:step! it))))))
        (is (<= bytes 32768) "3000 шагов сгенерированной модели выделили ~D байт" bytes)))))

(test generation-is-deterministic
  (let ((a (worm.codegen:compile-model 'worm.components:worm-3d :params "water-v1" :rings 6))
        (b (worm.codegen:compile-model 'worm.components:worm-3d :params "water-v1" :rings 6)))
    (is (string= (prin1-to-string (worm.codegen:model-forms a)) (prin1-to-string (worm.codegen:model-forms b))))))

(test check-state-reports-volume-collapse
  (let* ((gen (generated 'worm.components:worm-3d "water-v1" :rings 6))
         (p (worm.codegen:model-param-vector gen))
         (y (copy-seq (worm.codegen::model-layout-y0 (worm.core:compiled-model-layout gen)))))
    (is (null (funcall (worm.core:compiled-model-check-state gen) y p)))
    ;; сжать отсек 2 по сечению в 3 раза
    (loop for node from (* 2 8) below (* 4 8)
          do (setf (aref y (+ (* 3 node) 1)) (/ (aref y (+ (* 3 node) 1)) 3)
                   (aref y (+ (* 3 node) 2)) (/ (aref y (+ (* 3 node) 2)) 3)))
    (multiple-value-bind (cause where) (funcall (worm.core:compiled-model-check-state gen) y p)
      (is (eq :volume-collapse cause))
      (is (member where '(1 2 3))))))

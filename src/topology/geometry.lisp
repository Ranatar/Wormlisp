;;;; src/topology/geometry.lisp — объём отсека.
;;;;
;;;; Дизайн, «Гидростатика»: отсек между кольцами i и i+1 — замкнутый
;;;; многогранник из боковых четырёхугольников, разбитых на два
;;;; треугольника, и двух торцов-вееров вокруг центров колец (центр —
;;;; среднее узлов кольца). Объём — сумма x_a·(x_b × x_c)/6 по
;;;; треугольникам с внешней нормалью. Эта же функция даст объём на шаге;
;;;; её градиент и гессиан появятся на этапе 3.
;;;;
;;;; Ориентация в осях тела (x — к хвосту, y — дорсально, z — влево, угол
;;;; узла растёт от y к z): боковые треугольники (i,k)(i,k+1)(i+1,k) и
;;;; (i,k+1)(i+1,k+1)(i+1,k) смотрят наружу; передний торец — веер
;;;; (центр_i, k+1, k), задний — (центр_{i+1}, k, k+1).

(in-package #:worm.topology)

(defun ring-center (coords i ring-nodes)
  "Центр кольца i — среднее его узлов; три значения x, y, z."
  (declare (type dvec coords))
  (let ((x 0d0) (y 0d0) (z 0d0))
    (dotimes (k ring-nodes)
      (let ((p (* 3 (node-index i k ring-nodes))))
        (incf x (aref coords p)) (incf y (aref coords (+ p 1))) (incf z (aref coords (+ p 2)))))
    (values (/ x ring-nodes) (/ y ring-nodes) (/ z ring-nodes))))

(declaim (inline triple))
(defun triple (ax ay az bx by bz cx cy cz)
  "a · (b × c)."
  (+ (* ax (- (* by cz) (* bz cy)))
     (* ay (- (* bz cx) (* bx cz)))
     (* az (- (* bx cy) (* by cx)))))

(defun compartment-volume (coords c ring-nodes)
  "Объём отсека C (между кольцами C и C+1) при координатах узлов COORDS."
  (declare (type dvec coords))
  (let ((sum 0d0) (k-count ring-nodes))
    (labels ((node (i k)
               (let ((p (* 3 (node-index i (mod k k-count) k-count))))
                 (values (aref coords p) (aref coords (+ p 1)) (aref coords (+ p 2)))))
             (tri (ax ay az bx by bz cx cy cz)
               (incf sum (triple ax ay az bx by bz cx cy cz))))
      (dotimes (k k-count)
        (multiple-value-bind (ax ay az) (node c k)
          (multiple-value-bind (bx by bz) (node c (1+ k))
            (multiple-value-bind (cx cy cz) (node (1+ c) k)
              (multiple-value-bind (dx dy dz) (node (1+ c) (1+ k))
                (tri ax ay az bx by bz cx cy cz)
                (tri bx by bz dx dy dz cx cy cz))))))
      (multiple-value-bind (ox oy oz) (ring-center coords c k-count)
        (dotimes (k k-count)
          (multiple-value-bind (ax ay az) (node c k)
            (multiple-value-bind (bx by bz) (node c (1+ k))
              (tri ox oy oz bx by bz ax ay az)))))
      (multiple-value-bind (ox oy oz) (ring-center coords (1+ c) k-count)
        (dotimes (k k-count)
          (multiple-value-bind (ax ay az) (node (1+ c) k)
            (multiple-value-bind (bx by bz) (node (1+ c) (1+ k))
              (tri ox oy oz ax ay az bx by bz))))))
    (/ sum 6d0)))

(defun frustum-volume (r1 r2 h ring-nodes)
  "Объём усечённой пирамиды между правильными K-угольниками радиусов R1 и R2
на расстоянии H: h/3·(A₁ + A₂ + √(A₁A₂)), A = (K/2)·R²·sin(2π/K). Это
независимая проверка COMPARTMENT-VOLUME для трубки в покое, где кольца
соосны, параллельны и одинаково повёрнуты, а боковые грани плоские."
  (flet ((area (r) (* 0.5d0 ring-nodes (sq r) (sin (/ (* 2 +pi+) ring-nodes)))))
    (let ((a1 (area r1)) (a2 (area r2)))
      (* (/ h 3d0) (+ a1 a2 (sqrt (* a1 a2)))))))

;;; Центры колец и кривизна средней линии. Нужны ручной модели, анализу
;;; прогонов и (с этапа 7) сгенерированным наблюдаемым; память не выделяют.

(defun ring-centers (y rings ring-nodes centers)
  (declare (type dvec y centers) (type index rings ring-nodes) (optimize speed))
  (fill centers 0d0)
  (let ((inv (/ 1d0 ring-nodes)))
    (dotimes (i rings)
      (dotimes (k ring-nodes)
        (let ((p (* 3 (+ (* i ring-nodes) k))))
          (incf (aref centers (* 3 i)) (* inv (aref y p)))
          (incf (aref centers (+ (* 3 i) 1)) (* inv (aref y (+ p 1))))
          (incf (aref centers (+ (* 3 i) 2)) (* inv (aref y (+ p 2))))))))
  (values))

(defun ring-curvatures (y rings ring-nodes centers out)
  "Знаковая дорсо-вентральная кривизна в кольцах, 1/мм: положительна, когда
центр кривизны лежит с дорсальной стороны. На крайних кольцах — 0."
  (declare (type dvec y centers out) (type index rings ring-nodes) (optimize speed))
  (ring-centers y rings ring-nodes centers)
  (setf (aref out 0) 0d0 (aref out (1- rings)) 0d0)
  (loop for i from 1 below (1- rings)
        do (let* ((i0 (* 3 (1- i))) (i1 (* 3 i)) (i2 (* 3 (1+ i)))
                  (ax (- (aref centers i1) (aref centers i0)))
                  (ay (- (aref centers (+ i1 1)) (aref centers (+ i0 1))))
                  (az (- (aref centers (+ i1 2)) (aref centers (+ i0 2))))
                  (bx (- (aref centers i2) (aref centers i1)))
                  (by (- (aref centers (+ i2 1)) (aref centers (+ i1 1))))
                  (bz (- (aref centers (+ i2 2)) (aref centers (+ i1 2))))
                  (la (sqrt (+ (* ax ax) (* ay ay) (* az az))))
                  (lb (sqrt (+ (* bx bx) (* by by) (* bz bz))))
                  (scale (/ 2d0 (+ la lb)))
                  (kx (* scale (- (/ bx lb) (/ ax la))))
                  (ky (* scale (- (/ by lb) (/ ay la))))
                  (kz (* scale (- (/ bz lb) (/ az la))))
                  (dx 0d0) (dy 0d0) (dz 0d0))
             (declare (type fixnum i0 i1 i2)
                      (type double-float ax ay az bx by bz la lb scale kx ky kz dx dy dz))
             ;; дорсальное направление кольца: Σ cos θ_k·(x_k − центр)
             (dotimes (k ring-nodes)
               (let ((p (* 3 (+ (* i ring-nodes) k)))
                     (w (cos (/ (* (+ (* 2 k) 1) pi) ring-nodes))))
                 (declare (type fixnum p) (type double-float w))
                 (incf dx (* w (- (aref y p) (aref centers i1))))
                 (incf dy (* w (- (aref y (+ p 1)) (aref centers (+ i1 1)))))
                 (incf dz (* w (- (aref y (+ p 2)) (aref centers (+ i1 2)))))))
             (let ((dn (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
               (setf (aref out i) (/ (+ (* kx dx) (* ky dy) (* kz dz)) dn)))))
  (values))

;;; Объём отсека как функция координат его 2K узлов, с градиентом и
;;; гессианом. Треугольники — те же, что у COMPARTMENT-VOLUME: боковые
;;; (k, k+1, K+k), (k+1, K+k+1, K+k) и торцевые веера вокруг центров колец.
;;; Для T = a·(b×c): ∂T/∂a = b×c, ∂²T/∂a∂b = −[c]×, ∂²T/∂a∂c = [b]×,
;;; ∂²T/∂b∂c = −[a]×; вклад центра кольца переносится на его узлы с весом
;;; 1/K. Это общий код для любого K — значение примитивов volume,
;;; volume-grad и volume-hess языка модели (решение 21); быстрый вариант
;;; без выделения памяти — в ручных ядрах tests/oracle/.

(defun compartment-volume-derivatives (points ring-nodes &key hessian)
  "POINTS — 6K координат: K узлов переднего кольца, затем K заднего.
Значения: объём, градиент (6K) и, при HESSIAN, гессиан (6K × 6K)."
  (let* ((kk ring-nodes)
         (n (+ (* 2 kk) 2))
         (pts (make-array (* 3 n) :element-type 'double-float :initial-element 0d0))
         (gext (make-array (* 3 n) :element-type 'double-float :initial-element 0d0))
         (hext (when hessian (make-array (list (* 3 n) (* 3 n)) :element-type 'double-float
                                                                  :initial-element 0d0)))
         (volume 0d0))
    ;; координаты относительно центра переднего кольца
    (let ((o (loop for c below 3 collect (/ (loop for k below kk sum (elt points (+ (* 3 k) c))) kk))))
      (dotimes (j (* 2 kk))
        (dotimes (c 3) (setf (aref pts (+ (* 3 j) c)) (- (elt points (+ (* 3 j) c)) (nth c o))))))
    (dotimes (r 2)
      (dotimes (c 3)
        (setf (aref pts (+ (* 3 (+ (* 2 kk) r)) c))
              (/ (loop for k below kk sum (aref pts (+ (* 3 (+ (* r kk) k)) c))) kk))))
    (labels ((p (j c) (aref pts (+ (* 3 j) c)))
             (skew (row col v scale)
               ;; H[3row…, 3col…] += scale·[v]×
               (let ((r (* 3 row)) (c (* 3 col)))
                 (decf (aref hext r (+ c 1)) (* scale (third v))) (incf (aref hext r (+ c 2)) (* scale (second v)))
                 (incf (aref hext (+ r 1) c) (* scale (third v))) (decf (aref hext (+ r 1) (+ c 2)) (* scale (first v)))
                 (decf (aref hext (+ r 2) c) (* scale (second v))) (incf (aref hext (+ r 2) (+ c 1)) (* scale (first v)))))
             (cross (a b) (list (- (* (second a) (third b)) (* (third a) (second b)))
                                (- (* (third a) (first b)) (* (first a) (third b)))
                                (- (* (first a) (second b)) (* (second a) (first b)))))
             (tri (ia ib ic)
               (let* ((a (list (p ia 0) (p ia 1) (p ia 2)))
                      (b (list (p ib 0) (p ib 1) (p ib 2)))
                      (c (list (p ic 0) (p ic 1) (p ic 2)))
                      (bc (cross b c)) (ca (cross c a)) (ab (cross a b)))
                 (incf volume (/ (reduce #'+ (mapcar #'* a bc)) 6d0))
                 (dotimes (q 3)
                   (incf (aref gext (+ (* 3 ia) q)) (/ (nth q bc) 6d0))
                   (incf (aref gext (+ (* 3 ib) q)) (/ (nth q ca) 6d0))
                   (incf (aref gext (+ (* 3 ic) q)) (/ (nth q ab) 6d0)))
                 (when hessian
                   (let ((s (/ 1d0 6d0)))
                     (skew ia ib c (- s)) (skew ib ia c s)
                     (skew ia ic b s) (skew ic ia b (- s))
                     (skew ib ic a (- s)) (skew ic ib a s))))))
      (dotimes (k kk)
        (let ((k1 (mod (1+ k) kk)))
          (tri k k1 (+ kk k))
          (tri k1 (+ kk k1) (+ kk k))
          (tri (* 2 kk) k1 k)
          (tri (1+ (* 2 kk)) (+ kk k) (+ kk k1)))))
    (let* ((m (* 6 kk))
           (grad (make-array m :element-type 'double-float))
           (hess (when hessian (make-array (list m m) :element-type 'double-float)))
           (inv (/ 1d0 kk)))
      (dotimes (j (* 2 kk))
        (let ((o (+ (* 2 kk) (floor j kk))))
          (dotimes (q 3)
            (setf (aref grad (+ (* 3 j) q)) (+ (aref gext (+ (* 3 j) q)) (* inv (aref gext (+ (* 3 o) q))))))))
      (when hessian
        (dotimes (j (* 2 kk))
          (dotimes (m2 (* 2 kk))
            (let ((oj (+ (* 2 kk) (floor j kk))) (om (+ (* 2 kk) (floor m2 kk))))
              (dotimes (r 3)
                (dotimes (s 3)
                  (setf (aref hess (+ (* 3 j) r) (+ (* 3 m2) s))
                        (+ (aref hext (+ (* 3 j) r) (+ (* 3 m2) s))
                           (* inv (+ (aref hext (+ (* 3 j) r) (+ (* 3 om) s))
                                     (aref hext (+ (* 3 oj) r) (+ (* 3 m2) s))))
                           (* inv inv (aref hext (+ (* 3 oj) r) (+ (* 3 om) s)))))))))))
      (values volume grad hess))))

;;; Быстрая геометрия отсека для сгенерированного кода: объём, градиент и
;;; (по желанию) гессиан по узлам NODES (2K номеров: переднее кольцо, затем
;;; заднее) без выделения памяти — рабочие массивы в VOLUME-SCRATCH.
;;; Алгоритм тот же, что у COMPARTMENT-VOLUME-DERIVATIVES; координаты
;;; берутся относительно центра переднего кольца.

(defstruct (volume-scratch (:constructor make-volume-scratch
                               (ring-nodes &aux (n (* 3 (+ (* 2 ring-nodes) 2)))
                                                (m (* 6 ring-nodes))))
                           (:copier nil))
  (pts (make-dvec n) :type dvec)
  (gext (make-dvec n) :type dvec)
  (hext (make-array (list n n) :element-type 'double-float) :type (simple-array double-float (* *)))
  (grad (make-dvec m) :type dvec)
  (hess (make-array (list m m) :element-type 'double-float) :type (simple-array double-float (* *)))
  (volume (make-dvec 1) :type dvec))

(declaim (inline %skew))
(defun %skew (h row col v0 v1 v2 scale)
  (declare (type (simple-array double-float (* *)) h) (type fixnum row col)
           (type double-float v0 v1 v2 scale))
  (let ((r (* 3 row)) (c (* 3 col)))
    (declare (type fixnum r c))
    (decf (aref h r (+ c 1)) (* scale v2)) (incf (aref h r (+ c 2)) (* scale v1))
    (incf (aref h (+ r 1) c) (* scale v2)) (decf (aref h (+ r 1) (+ c 2)) (* scale v0))
    (decf (aref h (+ r 2) c) (* scale v1)) (incf (aref h (+ r 2) (+ c 1)) (* scale v0)))
  (values))

(defun compartment-geometry! (y nodes start ring-nodes scratch with-hessian)
  "Объём, градиент и гессиан отсека в SCRATCH; номера его 2K узлов — в NODES
с позиции START. Ничего не возвращает."
  (declare (type dvec y) (type (simple-array fixnum (*)) nodes) (type index start ring-nodes)
           (type volume-scratch scratch) (optimize (speed 3) (safety 0)))
  (let* ((kk ring-nodes)
         (pts (volume-scratch-pts scratch)) (gext (volume-scratch-gext scratch))
         (hext (volume-scratch-hext scratch))
         (volume 0d0) (sixth (/ 1d0 6d0)))
    (declare (type fixnum kk) (type double-float volume sixth))
    (let ((ox 0d0) (oy 0d0) (oz 0d0))
      (declare (type double-float ox oy oz))
      (dotimes (k kk)
        (let ((p (* 3 (aref nodes (+ start k)))))
          (declare (type fixnum p))
          (incf ox (aref y p)) (incf oy (aref y (+ p 1))) (incf oz (aref y (+ p 2)))))
      (setf ox (/ ox kk) oy (/ oy kk) oz (/ oz kk))
      (dotimes (j (* 2 kk))
        (let ((p (* 3 (aref nodes (+ start j)))))
          (declare (type fixnum p))
          (setf (aref pts (* 3 j)) (- (aref y p) ox)
                (aref pts (+ (* 3 j) 1)) (- (aref y (+ p 1)) oy)
                (aref pts (+ (* 3 j) 2)) (- (aref y (+ p 2)) oz))))
      (dotimes (r 2)
        (let ((cx 0d0) (cy 0d0) (cz 0d0))
          (declare (type double-float cx cy cz))
          (dotimes (k kk)
            (let ((j (+ (* r kk) k)))
              (incf cx (aref pts (* 3 j))) (incf cy (aref pts (+ (* 3 j) 1))) (incf cz (aref pts (+ (* 3 j) 2)))))
          (let ((o (* 3 (+ (* 2 kk) r))))
            (setf (aref pts o) (/ cx kk) (aref pts (+ o 1)) (/ cy kk) (aref pts (+ o 2)) (/ cz kk))))))
    (fill gext 0d0)
    (when with-hessian (fill (sb-ext:array-storage-vector hext) 0d0))
    (macrolet ((tri (ia-form ib-form ic-form)
                 `(let* ((ia ,ia-form) (ib ,ib-form) (ic ,ic-form)
                         (a (* 3 ia)) (b (* 3 ib)) (cc (* 3 ic))
                         (ax (aref pts a)) (ay (aref pts (+ a 1))) (az (aref pts (+ a 2)))
                         (bx (aref pts b)) (by (aref pts (+ b 1))) (bz (aref pts (+ b 2)))
                         (cx (aref pts cc)) (cy (aref pts (+ cc 1))) (cz (aref pts (+ cc 2)))
                         (bcx (- (* by cz) (* bz cy))) (bcy (- (* bz cx) (* bx cz))) (bcz (- (* bx cy) (* by cx)))
                         (cax (- (* cy az) (* cz ay))) (cay (- (* cz ax) (* cx az))) (caz (- (* cx ay) (* cy ax)))
                         (abx (- (* ay bz) (* az by))) (aby (- (* az bx) (* ax bz))) (abz (- (* ax by) (* ay bx))))
                    (declare (type fixnum ia ib ic a b cc)
                             (type double-float ax ay az bx by bz cx cy cz bcx bcy bcz cax cay caz abx aby abz))
                    (incf volume (* sixth (+ (* ax bcx) (* ay bcy) (* az bcz))))
                    (incf (aref gext a) (* sixth bcx)) (incf (aref gext (+ a 1)) (* sixth bcy))
                    (incf (aref gext (+ a 2)) (* sixth bcz))
                    (incf (aref gext b) (* sixth cax)) (incf (aref gext (+ b 1)) (* sixth cay))
                    (incf (aref gext (+ b 2)) (* sixth caz))
                    (incf (aref gext cc) (* sixth abx)) (incf (aref gext (+ cc 1)) (* sixth aby))
                    (incf (aref gext (+ cc 2)) (* sixth abz))
                    (when with-hessian
                      (%skew hext ia ib cx cy cz (- sixth)) (%skew hext ib ia cx cy cz sixth)
                      (%skew hext ia ic bx by bz sixth) (%skew hext ic ia bx by bz (- sixth))
                      (%skew hext ib ic ax ay az (- sixth)) (%skew hext ic ib ax ay az sixth)))))
      (dotimes (k kk)
        (let ((k1 (if (= (1+ k) kk) 0 (1+ k))))
          (declare (type fixnum k1))
          (tri k k1 (+ kk k))
          (tri k1 (+ kk k1) (+ kk k))
          (tri (* 2 kk) k1 k)
          (tri (1+ (* 2 kk)) (+ kk k) (+ kk k1)))))
    (let ((grad (volume-scratch-grad scratch))
          (hess (volume-scratch-hess scratch))
          (inv (/ 1d0 kk)))
      (declare (type double-float inv))
      (dotimes (j (* 2 kk))
        (let ((o (+ (* 2 kk) (floor j kk))))
          (dotimes (q 3)
            (setf (aref grad (+ (* 3 j) q))
                  (+ (aref gext (+ (* 3 j) q)) (* inv (aref gext (+ (* 3 o) q))))))))
      (when with-hessian
        (dotimes (j (* 2 kk))
          (let ((oj (+ (* 2 kk) (floor j kk))))
            (dotimes (m (* 2 kk))
              (let ((om (+ (* 2 kk) (floor m kk))))
                (dotimes (r 3)
                  (dotimes (s 3)
                    (let ((jr (+ (* 3 j) r)) (ms (+ (* 3 m) s))
                          (ojr (+ (* 3 oj) r)) (oms (+ (* 3 om) s)))
                      (setf (aref hess jr ms)
                            (+ (aref hext jr ms)
                               (* inv (+ (aref hext jr oms) (aref hext ojr ms)))
                               (* inv inv (aref hext ojr oms))))))))))))
      (setf (aref (volume-scratch-volume scratch) 0) volume)))
  (values))

;;; Натяжения покоя при предварительном давлении (параметры, «Гидростатика»).

(defun prestress-tensions (tube)
  "Натяжения рёбер прямого тела покоя при единичном давлении: значения
(окружные по кольцам, продольные по промежуткам, диагональные по рёбрам,
невязка). Осевую силу давления гасят продольные рёбра — по цепочке от
головы, радиальную — окружные, касательную на конических концах —
диагонали двух закруток (+D и −D). Невязка — наибольшая оставшаяся сила на
узел в долях наибольшей силы давления."
  (let* ((kk (tube-ring-nodes tube))
         (rings (tube-rings tube))
         (y (tube-rest-coords tube))
         (g (make-dvec (length y)))
         (circ (make-dvec rings))
         (long (make-dvec (1- rings)))
         (twist (make-dvec (1- rings)))
         (centers (make-dvec (* 3 rings))))
    (dotimes (c (1- rings))
      (let ((grad (nth-value 1 (compartment-volume-derivatives
                                (subseq y (* 3 c kk) (* 3 (+ c 2) kk)) kk))))
        (dotimes (q (* 6 kk)) (incf (aref g (+ (* 3 c kk) q)) (aref grad q)))))
    (ring-centers y rings kk centers)
    (labels ((node (i k) (let ((p (* 3 (+ (* i kk) k)))) (list (aref y p) (aref y (+ p 1)) (aref y (+ p 2)))))
             (center (i) (list (aref centers (* 3 i)) (aref centers (+ (* 3 i) 1)) (aref centers (+ (* 3 i) 2))))
             (unit (v) (let ((n (sqrt (reduce #'+ (mapcar #'sq v))))) (mapcar (lambda (x) (/ x n)) v)))
             (dot (a b) (reduce #'+ (mapcar #'* a b)))
             (radial (i k) (unit (mapcar #'- (node i k) (center i))))
             (around (i k) (let ((r (radial i k))) (list 0d0 (- (third r)) (second r))))
             (force (i) (let ((p (* 3 (* i kk)))) (list (aref g p) (aref g (+ p 1)) (aref g (+ p 2))))))
      (let* ((ex '(1d0 0d0 0d0)) (sin-k (sin (/ pi kk)))
             (prev-t 0d0) (prev-u ex) (prev-d 0d0) (prev-dt-hi 0d0))
        (dotimes (i rings)
          (let* ((f (force i)) (er (radial i 0)) (et (around i 0))
                 (last (= i (1- rings)))
                 (u (if last ex (unit (mapcar #'- (node (1+ i) 0) (node i 0)))))
                 (dt (if last 0d0 (abs (dot (unit (mapcar #'- (node (1+ i) 1) (node i 0))) et))))
                 (dt-hi (if last 0d0 (abs (dot (unit (mapcar #'- (node i 0) (node (1+ i) 1))) (around (1+ i) 1)))))
                 (ti (if last 0d0 (/ (- (* prev-t (dot prev-u ex)) (dot f ex)) (dot u ex))))
                 (di (if last 0d0 (/ (- (* 2 prev-dt-hi prev-d) (dot f et)) (* 2 dt)))))
            (unless last (setf (aref long i) ti (aref twist i) di))
            (setf (aref circ i) (/ (+ (dot f er) (* ti (dot u er)) (- (* prev-t (dot prev-u er)))) (* 2 sin-k)))
            (setf prev-t ti prev-u u prev-d di prev-dt-hi dt-hi))))
      (multiple-value-bind (a b) (family-ends tube :diagonal)
        (let ((diag (make-dvec (length a))))
          (dotimes (e (length a))
            (let* ((lo (min (aref a e) (aref b e))) (hi (max (aref a e) (aref b e)))
                   (i (floor lo kk)) (k (mod lo kk))
                   (d (mapcar #'- (node (floor hi kk) (mod hi kk)) (node i k))))
              (setf (aref diag e) (* (if (plusp (dot d (around i k))) 1d0 -1d0) (aref twist i)))))
          ;; невязка
          (let ((check (copy-seq g)))
            (dolist (fam '(:circ-edge :long-edge :diagonal))
              (multiple-value-bind (fa fb) (family-ends tube fam)
                (progn
                  (dotimes (e (length fa))
                    (let* ((tension (ecase fam
                                      (:circ-edge (aref circ (floor e kk)))
                                      (:long-edge (aref long (floor e kk)))
                                      (:diagonal (aref diag e))))
                           (pa (* 3 (aref fa e))) (pb (* 3 (aref fb e)))
                           (dv (loop for c below 3 collect (- (aref y (+ pb c)) (aref y (+ pa c)))))
                           (l (sqrt (reduce #'+ (mapcar #'sq dv)))))
                      (loop for c below 3 for d in dv
                            do (incf (aref check (+ pa c)) (* tension (/ d l)))
                               (decf (aref check (+ pb c)) (* tension (/ d l)))))))))
            (flet ((largest (v) (loop for i below (length v) by 3
                                      maximize (sqrt (+ (sq (aref v i)) (sq (aref v (+ i 1))) (sq (aref v (+ i 2))))))))
              (values circ long diag (/ (largest check) (largest g))))))))))

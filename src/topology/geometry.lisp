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

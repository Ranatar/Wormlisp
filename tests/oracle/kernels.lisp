;;;; tests/oracle/kernels.lisp — ручные ядра тела и среды.
;;;;
;;;; Соглашения (worm.core, model.lisp): силы прибавляются к G, а матрица
;;;; шага A = B − h·∂G/∂y — к ленте ab. Координата c узла n лежит в
;;;; y[3n + c]. Механика передемпфирована: B = M + D, где M — сопротивление
;;;; среды, D — вязкость рёбер (дизайн, «Уравнение движения»).
;;;;
;;;; Ядра не выделяют память: рабочие массивы передаются снаружи.

(in-package #:worm.oracle)

(declaim (optimize (speed 3) (safety 1) (debug 0)))

(defmacro band+ (ab row col kl ku value)
  `(incf (aref ,ab (band-index ,row ,col ,kl ,ku)) ,value))

;;; Рёбра
;;;
;;; Сила вдоль ребра f = k·(ℓ − ℓ₀) + T, где T — натяжение, приложенное к
;;; ребру мышцами (только у продольных). Узел a тянется к b с силой f·u,
;;; узел b — к a. Блок якобиана ∂(f·u)/∂x_b = k·uuᵀ + (f/ℓ)·(I − uuᵀ);
;;; вязкость η·ℓ̇·u даёт в D блок η·uuᵀ.

(defun edge-forces (y a b rest k tension g)
  "Упругие силы и натяжение рёбер (концы A, B) прибавить к G."
  (declare (type dvec y rest k g) (type ivec a b) (type (or null dvec) tension))
  (dotimes (e (length a))
    (let* ((pa (* 3 (aref a e))) (pb (* 3 (aref b e)))
           (dx (- (aref y pb) (aref y pa)))
           (dy (- (aref y (+ pb 1)) (aref y (+ pa 1))))
           (dz (- (aref y (+ pb 2)) (aref y (+ pa 2))))
           (l (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
           (f (+ (* (aref k e) (- l (aref rest e))) (if tension (aref tension e) 0d0)))
           (s (/ f l)))
      (declare (type fixnum pa pb) (type double-float dx dy dz l f s))
      (incf (aref g pa) (* s dx)) (incf (aref g (+ pa 1)) (* s dy)) (incf (aref g (+ pa 2)) (* s dz))
      (decf (aref g pb) (* s dx)) (decf (aref g (+ pb 1)) (* s dy)) (decf (aref g (+ pb 2)) (* s dz))))
  (values))

(defun edge-matrix (y a b rest k eta tension h ab kl ku)
  "Вклад рёбер в A = B − h·J: блок M = (h·k + η − h·f/ℓ)·uuᵀ + (h·f/ℓ)·I
на диагональные блоки концов и −M на внедиагональные."
  (declare (type dvec y rest k eta ab) (type ivec a b) (type (or null dvec) tension)
           (type double-float h) (type index kl ku))
  (dotimes (e (length a))
    (let* ((pa (* 3 (aref a e))) (pb (* 3 (aref b e)))
           (dx (- (aref y pb) (aref y pa)))
           (dy (- (aref y (+ pb 1)) (aref y (+ pa 1))))
           (dz (- (aref y (+ pb 2)) (aref y (+ pa 2))))
           (l (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
           (ux (/ dx l)) (uy (/ dy l)) (uz (/ dz l))
           (f (+ (* (aref k e) (- l (aref rest e))) (if tension (aref tension e) 0d0)))
           (c2 (/ (* h f) l))
           (c1 (- (+ (* h (aref k e)) (aref eta e)) c2)))
      (declare (type fixnum pa pb) (type double-float dx dy dz l ux uy uz f c1 c2))
      (macrolet ((block-entries ()
                   `(progn
                      ,@(loop for (i ui) in '((0 ux) (1 uy) (2 uz))
                              append (loop for (j uj) in '((0 ux) (1 uy) (2 uz))
                                           collect `(let ((m (+ (* c1 ,ui ,uj) ,(if (= i j) 'c2 0d0))))
                                                      (band+ ab (+ pa ,i) (+ pa ,j) kl ku m)
                                                      (band+ ab (+ pb ,i) (+ pb ,j) kl ku m)
                                                      (band+ ab (+ pa ,i) (+ pb ,j) kl ku (- m))
                                                      (band+ ab (+ pb ,i) (+ pa ,j) kl ku (- m))))))))
        (block-entries))))
  (values))

;;; Касательные к средней линии (центры колец — worm.topology:ring-centers)

(defun ring-tangents (centers rings tangents)
  "Единичные касательные в кольцах по разности соседних центров."
  (declare (type dvec centers tangents) (type index rings))
  (dotimes (i rings)
    (let* ((lo (max 0 (1- i))) (hi (min (1- rings) (1+ i)))
           (tx (- (aref centers (* 3 hi)) (aref centers (* 3 lo))))
           (ty (- (aref centers (+ (* 3 hi) 1)) (aref centers (+ (* 3 lo) 1))))
           (tz (- (aref centers (+ (* 3 hi) 2)) (aref centers (+ (* 3 lo) 2))))
           (n (sqrt (+ (* tx tx) (* ty ty) (* tz tz)))))
      (declare (type double-float tx ty tz n))
      (setf (aref tangents (* 3 i)) (/ tx n)
            (aref tangents (+ (* 3 i) 1)) (/ ty n)
            (aref tangents (+ (* 3 i) 2)) (/ tz n))))
  (values))

;;; Сопротивление среды (дизайн, «Среда»): блок узла
;;; M = (C_t·ttᵀ + C_n·(I − ttᵀ))·Δs/K — в B, без множителя h.

(defun drag-matrix (tangents ring-ds rings ring-nodes c-t c-n ab kl ku)
  (declare (type dvec tangents ring-ds ab) (type index rings ring-nodes kl ku)
           (type double-float c-t c-n))
  (dotimes (i rings)
    (let* ((w (/ (aref ring-ds i) ring-nodes))
           (tx (aref tangents (* 3 i))) (ty (aref tangents (+ (* 3 i) 1)))
           (tz (aref tangents (+ (* 3 i) 2)))
           (d (* w (- c-t c-n))) (iso (* w c-n)))
      (declare (type double-float w tx ty tz d iso))
      (dotimes (k ring-nodes)
        (let ((p (* 3 (+ (* i ring-nodes) k))))
          (declare (type fixnum p))
          (macrolet ((entries ()
                       `(progn
                          ,@(loop for (r tr) in '((0 tx) (1 ty) (2 tz))
                                  append (loop for (c tc) in '((0 tx) (1 ty) (2 tz))
                                               collect `(band+ ab (+ p ,r) (+ p ,c) kl ku
                                                               (+ (* d ,tr ,tc) ,(if (= r c) 'iso 0d0))))))))
            (entries))))))
  (values))

;;; Контакт с поверхностью z = 0 (дизайн, «Контакт с поверхностью»):
;;; F_z = k_s·max(0, −z) − f_m·Δs/K.

(defun contact-forces (y ring-ds rings ring-nodes k-s f-m g)
  (declare (type dvec y ring-ds g) (type index rings ring-nodes) (type double-float k-s f-m))
  (dotimes (i rings)
    (let ((pull (/ (* f-m (aref ring-ds i)) ring-nodes)))
      (declare (type double-float pull))
      (dotimes (k ring-nodes)
        (let* ((pz (+ (* 3 (+ (* i ring-nodes) k)) 2))
               (z (aref y pz)))
          (declare (type fixnum pz) (type double-float z))
          (incf (aref g pz) (- (if (< z 0d0) (* k-s (- z)) 0d0) pull))))))
  (values))

(defun contact-matrix (y n-nodes k-s h ab kl ku)
  (declare (type dvec y ab) (type index n-nodes kl ku) (type double-float k-s h))
  (dotimes (n n-nodes)
    (let ((pz (+ (* 3 n) 2)))
      (declare (type fixnum pz))
      (when (< (aref y pz) 0d0)
        (band+ ab pz pz kl ku (* h k-s)))))
  (values))

;;; Давление отсеков (дизайн, «Гидростатика»).
;;;
;;; Объём отсека — сумма a·(b×c)/6 по треугольникам: боковые
;;; (k, k+1, K+k), (k+1, K+k+1, K+k) и торцевые веера вокруг центров
;;; колец (2K, k+1, k), (2K+1, K+k, K+k+1) в локальной нумерации отсека:
;;; 0…K−1 — узлы переднего кольца, K…2K−1 — заднего, 2K и 2K+1 — центры.
;;; Градиент и гессиан объёма выписаны вручную по треугольникам, а не
;;; выведены из формулы объёма вслепую (архитектура, «Известное узкое
;;; место»): ∂T/∂a = b×c, ∂²T/∂a∂b = −[c]×, ∂²T/∂a∂c = [b]×, ∂²T/∂b∂c = −[a]×
;;; для T = a·(b×c). Центр кольца — среднее его узлов, поэтому вклад центров
;;; переносится на узлы с весом 1/K.
;;;
;;; Координаты берутся относительно центра переднего кольца: объём и его
;;; производные от сдвига не зависят, а точность выше.

(defstruct (pressure-scratch (:constructor make-pressure-scratch
                                 (ring-nodes &aux (n (* 3 (+ (* 2 ring-nodes) 2)))
                                                  (m (* 6 ring-nodes)))))
  (pts (make-dvec n) :type dvec)
  (gext (make-dvec n) :type dvec)
  (hext (make-dmat n n) :type dmat)
  (grad (make-dvec m) :type dvec)
  (hess (make-dmat m m) :type dmat))

(defun add-skew (h row col v0 v1 v2 scale)
  "H[3row…, 3col…] += scale·[v]×."
  (declare (type dmat h) (type fixnum row col) (type double-float v0 v1 v2 scale))
  (let ((r (* 3 row)) (c (* 3 col)))
    (declare (type fixnum r c))
    (decf (aref h r (+ c 1)) (* scale v2)) (incf (aref h r (+ c 2)) (* scale v1))
    (incf (aref h (+ r 1) c) (* scale v2)) (decf (aref h (+ r 1) (+ c 2)) (* scale v0))
    (decf (aref h (+ r 2) c) (* scale v1)) (incf (aref h (+ r 2) (+ c 1)) (* scale v0)))
  (values))

(defun compartment-geometry (y c ring-nodes scratch with-hessian)
  "Объём отсека C. В SCRATCH: grad — ∂V/∂x по 2K узлам отсека (6K чисел),
при WITH-HESSIAN — hess, матрица 6K × 6K вторых производных."
  (declare (type dvec y) (type index c ring-nodes) (type pressure-scratch scratch))
  (let* ((kk ring-nodes)
         (pts (pressure-scratch-pts scratch))
         (gext (pressure-scratch-gext scratch))
         (hext (pressure-scratch-hext scratch))
         (base (* 3 c kk))
         (volume 0d0)
         (sixth (/ 1d0 6d0)))
    (declare (type fixnum kk base) (type double-float volume sixth))
    ;; узлы двух колец подряд в y: локальная точка j ↔ y[base + 3j]
    (let ((ox 0d0) (oy 0d0) (oz 0d0))
      (declare (type double-float ox oy oz))
      (dotimes (k kk)
        (incf ox (aref y (+ base (* 3 k)))) (incf oy (aref y (+ base (* 3 k) 1)))
        (incf oz (aref y (+ base (* 3 k) 2))))
      (setf ox (/ ox kk) oy (/ oy kk) oz (/ oz kk))
      (dotimes (j (* 2 kk))
        (setf (aref pts (* 3 j)) (- (aref y (+ base (* 3 j))) ox)
              (aref pts (+ (* 3 j) 1)) (- (aref y (+ base (* 3 j) 1)) oy)
              (aref pts (+ (* 3 j) 2)) (- (aref y (+ base (* 3 j) 2)) oz)))
      ;; центры колец
      (dotimes (r 2)
        (let ((cx 0d0) (cy 0d0) (cz 0d0))
          (declare (type double-float cx cy cz))
          (dotimes (k kk)
            (let ((j (+ (* r kk) k)))
              (incf cx (aref pts (* 3 j))) (incf cy (aref pts (+ (* 3 j) 1)))
              (incf cz (aref pts (+ (* 3 j) 2)))))
          (let ((o (* 3 (+ (* 2 kk) r))))
            (setf (aref pts o) (/ cx kk) (aref pts (+ o 1)) (/ cy kk) (aref pts (+ o 2)) (/ cz kk))))))
    (fill gext 0d0)
    (when with-hessian (fill (sb-ext:array-storage-vector hext) 0d0))
    (flet ((tri (ia ib ic)
             (declare (type fixnum ia ib ic))
             (let* ((a (* 3 ia)) (b (* 3 ib)) (cc (* 3 ic))
                    (ax (aref pts a)) (ay (aref pts (+ a 1))) (az (aref pts (+ a 2)))
                    (bx (aref pts b)) (by (aref pts (+ b 1))) (bz (aref pts (+ b 2)))
                    (cx (aref pts cc)) (cy (aref pts (+ cc 1))) (cz (aref pts (+ cc 2)))
                    ;; b×c, c×a, a×b
                    (bcx (- (* by cz) (* bz cy))) (bcy (- (* bz cx) (* bx cz))) (bcz (- (* bx cy) (* by cx)))
                    (cax (- (* cy az) (* cz ay))) (cay (- (* cz ax) (* cx az))) (caz (- (* cx ay) (* cy ax)))
                    (abx (- (* ay bz) (* az by))) (aby (- (* az bx) (* ax bz))) (abz (- (* ax by) (* ay bx))))
               (declare (type fixnum a b cc)
                        (type double-float ax ay az bx by bz cx cy cz bcx bcy bcz cax cay caz abx aby abz))
               (incf volume (* sixth (+ (* ax bcx) (* ay bcy) (* az bcz))))
               (incf (aref gext a) (* sixth bcx)) (incf (aref gext (+ a 1)) (* sixth bcy))
               (incf (aref gext (+ a 2)) (* sixth bcz))
               (incf (aref gext b) (* sixth cax)) (incf (aref gext (+ b 1)) (* sixth cay))
               (incf (aref gext (+ b 2)) (* sixth caz))
               (incf (aref gext cc) (* sixth abx)) (incf (aref gext (+ cc 1)) (* sixth aby))
               (incf (aref gext (+ cc 2)) (* sixth abz))
               (when with-hessian
                 (add-skew hext ia ib cx cy cz (- sixth)) (add-skew hext ib ia cx cy cz sixth)
                 (add-skew hext ia ic bx by bz sixth) (add-skew hext ic ia bx by bz (- sixth))
                 (add-skew hext ib ic ax ay az (- sixth)) (add-skew hext ic ib ax ay az sixth))
               (values))))
      (declare (inline tri))
      (dotimes (k kk)
        (let ((k1 (mod (1+ k) kk)))
          (tri k k1 (+ kk k))
          (tri k1 (+ kk k1) (+ kk k))
          (tri (* 2 kk) k1 k)
          (tri (1+ (* 2 kk)) (+ kk k) (+ kk k1)))))
    ;; перенос вкладов центров на узлы
    (let ((grad (pressure-scratch-grad scratch))
          (hess (pressure-scratch-hess scratch))
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
      volume)))

(defun pressure-forces (y n-compartments ring-nodes rest-volume k-p scratch g)
  "Силы давления p·∂V/∂x, p = k_p·(V⁰ − V)/V⁰."
  (declare (type dvec y rest-volume g) (type index n-compartments ring-nodes)
           (type double-float k-p) (type pressure-scratch scratch))
  (let ((grad (pressure-scratch-grad scratch)))
    (dotimes (c n-compartments)
      (let* ((v0 (aref rest-volume c))
             (v (compartment-geometry y c ring-nodes scratch nil))
             (p (/ (* k-p (- v0 v)) v0))
             (base (* 3 c ring-nodes)))
        (declare (type double-float v0 v p) (type fixnum base))
        (dotimes (q (* 6 ring-nodes))
          (incf (aref g (+ base q)) (* p (aref grad q)))))))
  (values))

(defun pressure-matrix (y n-compartments ring-nodes rest-volume k-p scratch h ab kl ku)
  "Вклад давления в A: −h·(p·∇²V − (k_p/V⁰)·∇V ∇Vᵀ) на блок 6K × 6K отсека."
  (declare (type dvec y rest-volume ab) (type index n-compartments ring-nodes kl ku)
           (type double-float k-p h) (type pressure-scratch scratch))
  (let ((grad (pressure-scratch-grad scratch))
        (hess (pressure-scratch-hess scratch))
        (m (* 6 ring-nodes)))
    (declare (type fixnum m))
    (dotimes (c n-compartments)
      (let* ((v0 (aref rest-volume c))
             (v (compartment-geometry y c ring-nodes scratch t))
             (p (/ (* k-p (- v0 v)) v0))
             (stiff (/ k-p v0))
             (base (* 3 c ring-nodes)))
        (declare (type double-float v0 v p stiff) (type fixnum base))
        (dotimes (r m)
          (dotimes (s m)
            (band+ ab (+ base r) (+ base s) kl ku
                   (* (- h) (- (* p (aref hess r s)) (* stiff (aref grad r) (aref grad s))))))))))
  (values))

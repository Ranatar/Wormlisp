;;;; src/topology/tube.lisp — трубка N × K.
;;;;
;;;; Система координат тела в покое: ось x — средняя линия от головы
;;;; (s = 0) к хвосту (s = L), ось y — дорсальная сторона, ось z — левая.
;;;; Узел k кольца i стоит под углом θ_k = (2k + 1)·π/K от дорсальной
;;;; средней линии в сторону левого бока; при K = 8 это 22,5° + 45°·k
;;;; (дизайн, «Узлы»). Тогда узлы 0…K/4−1 — дорсальный левый квадрант,
;;;; далее вентральный левый, вентральный правый, дорсальный правый, а
;;;; дорсальная, вентральная и обе латеральные линии проходят между узлами.
;;;;
;;;; Номер узла — i·K + k, его координата c — по индексу 3(i·K + k) + c
;;;; (архитектура, «Топология трубки»). Порядок элементов в каждом семействе
;;;; — по кольцам, затем по узлам: так связанные элементы лежат рядом.

(in-package #:worm.topology)

(defparameter +families+ '(:long-edge :circ-edge :diagonal :diametral)
  "Семейства рёбер в порядке архитектуры.")

(defparameter +quadrants+ '(:dorsal-left :ventral-left :ventral-right :dorsal-right)
  "Квадранты в порядке обхода узлов кольца.")

;;; Профиль радиуса

(defun ellipse-semi-axis (length radius-max radius-end-min)
  "Большая полуось a эллипса профиля, при которой радиус крайних колец
равен RADIUS-END-MIN (параметры, «Геометрия тела»)."
  (assert (< 0 radius-end-min radius-max))
  (/ (* 0.5d0 length) (sqrt (- 1d0 (sq (/ radius-end-min radius-max))))))

(defun ellipse-radius (s length radius-max semi-axis)
  "R(s) = R·√(1 − ((s − L/2)/a)²)."
  (* radius-max (sqrt (max 0d0 (- 1d0 (sq (/ (- s (* 0.5d0 length)) semi-axis)))))))

;;; Узлы и квадранты

(declaim (inline node-index long-edge-index))
(defun node-index (i k ring-nodes) (+ (* i ring-nodes) k))
(defun long-edge-index (i k ring-nodes)
  "Номер продольного ребра между узлом k кольца i и узлом k кольца i+1."
  (+ (* i ring-nodes) k))

(defun node-angle (k ring-nodes)
  "Угол узла k от дорсальной средней линии к левому боку, рад."
  (/ (* (+ (* 2 k) 1) +pi+) ring-nodes))

(defun node-quadrant (k ring-nodes)
  (nth (floor (* 4 k) ring-nodes) +quadrants+))

(defun quadrant-nodes (quadrant ring-nodes)
  "Номера узлов кольца в квадранте QUADRANT, по возрастанию угла."
  (loop for k below ring-nodes
        when (eq (node-quadrant k ring-nodes) quadrant) collect k))

(defun node-row (k ring-nodes)
  "Ряд узла в своём квадранте: :medial — ближе к дорсальной или вентральной
средней линии, :lateral — ближе к боковой линии; при K > 8 — :inner для средних."
  (let* ((per (/ ring-nodes 4))
         (pos (mod k per))
         ;; в левых квадрантах угол растёт от дорсальной линии, поэтому
         ;; медиальный узел дорсального левого — первый, вентрального левого —
         ;; последний; справа наоборот.
         (from-midline (ecase (node-quadrant k ring-nodes)
                         ((:dorsal-left :ventral-right) pos)
                         ((:ventral-left :dorsal-right) (- per 1 pos)))))
    (cond ((= from-midline 0) :medial)
          ((= from-midline (1- per)) :lateral)
          (t :inner))))

;;; Структура трубки

(defstruct (tube (:constructor %make-tube) (:copier nil))
  (rings 0 :type index :read-only t)
  (ring-nodes 0 :type index :read-only t)
  (length 0d0 :type double-float :read-only t)
  (radius-max 0d0 :type double-float :read-only t)
  (radius-end-min 0d0 :type double-float :read-only t)
  (semi-axis 0d0 :type double-float :read-only t)
  (ring-s nil :type dvec :read-only t)  ; Положение кольца вдоль тела, мм.
  (ring-radius nil :type dvec :read-only t)  ; Радиус покоя кольца, мм.
  (ring-ds nil :type dvec :read-only t)  ; Длина участка тела, приходящаяся на кольцо, мм.
  (rest-coords nil :type dvec :read-only t)  ; Координаты узлов в покое, 3NK.
  (ends nil :read-only t)  ; plist семейство → (a b): концы рёбер, ivec.
  (rest nil :read-only t)  ; plist семейство → длины покоя, dvec.
  (compartment-ring nil :type ivec :read-only t)  ; Переднее кольцо отсека.
  (rest-volume nil :type dvec :read-only t))  ; Объём покоя отсека, мм³.

(defmethod print-object ((tb tube) stream)
  (print-unreadable-object (tb stream :type t)
    (format stream "~D×~D, L = ~F мм, R = ~F мм" (tube-rings tb) (tube-ring-nodes tb)
            (tube-length tb) (tube-radius-max tb))))

(defun tube-node-count (tube) (* (tube-rings tube) (tube-ring-nodes tube)))

(defun tube-ring-fractions (tube)
  "Положения колец как доли длины тела."
  (let ((l (tube-length tube)))
    (map 'dvec (lambda (s) (/ s l)) (tube-ring-s tube))))

(defun family-ends (tube family)
  "Два ivec — первые и вторые концы рёбер семейства FAMILY."
  (values-list (or (getf (tube-ends tube) family)
                   (error "неизвестное семейство рёбер ~S" family))))

(defun family-rest (tube family)
  (or (getf (tube-rest tube) family)
      (error "неизвестное семейство рёбер ~S" family)))

(defun family-size (tube family)
  "Число элементов семейства: рёбра, а также :compartment и :ring."
  (case family
    (:compartment (1- (tube-rings tube)))
    (:ring (tube-rings tube))
    (t (length (family-rest tube family)))))

(defun quadrant-long-edges (tube quadrant i)
  "Продольные рёбра квадранта QUADRANT между кольцами i и i+1."
  (let ((k-count (tube-ring-nodes tube)))
    (mapcar (lambda (k) (long-edge-index i k k-count)) (quadrant-nodes quadrant k-count))))

;;; Построение

(defun edge-lengths (coords a b)
  (declare (type dvec coords) (type ivec a b))
  (let ((out (make-dvec (length a))))
    (dotimes (e (length a) out)
      (let ((pa (* 3 (aref a e))) (pb (* 3 (aref b e))))
        (setf (aref out e)
              (sqrt (+ (sq (- (aref coords pb) (aref coords pa)))
                       (sq (- (aref coords (+ pb 1)) (aref coords (+ pa 1))))
                       (sq (- (aref coords (+ pb 2)) (aref coords (+ pa 2)))))))))))

(defun build-edges (rings k-count)
  "plist семейство → (a b) для всех четырёх семейств."
  (flet ((collect (generator)
           (let ((as '()) (bs '()))
             (funcall generator (lambda (a b) (push a as) (push b bs)))
             (list (ivec-from (nreverse as)) (ivec-from (nreverse bs))))))
    (list
     :long-edge
     (collect (lambda (add)
                (dotimes (i (1- rings))
                  (dotimes (k k-count)
                    (funcall add (node-index i k k-count) (node-index (1+ i) k k-count))))))
     :circ-edge
     (collect (lambda (add)
                (dotimes (i rings)
                  (dotimes (k k-count)
                    (funcall add (node-index i k k-count)
                             (node-index i (mod (1+ k) k-count) k-count))))))
     :diagonal
     ;; на каждом четырёхугольнике боковой стенки — обе диагонали
     (collect (lambda (add)
                (dotimes (i (1- rings))
                  (dotimes (k k-count)
                    (funcall add (node-index i k k-count)
                             (node-index (1+ i) (mod (1+ k) k-count) k-count))
                    (funcall add (node-index i (mod (1+ k) k-count) k-count)
                             (node-index (1+ i) k k-count))))))
     :diametral
     (collect (lambda (add)
                (dotimes (i rings)
                  (dotimes (k (/ k-count 2))
                    (funcall add (node-index i k k-count)
                             (node-index i (+ k (/ k-count 2)) k-count)))))))))

(defun make-tube (&key (rings 50) (ring-nodes 8) (length 1d0) (radius-max 0.04d0)
                       (radius-end-min 0.005d0))
  "Трубка из RINGS колец по RING-NODES узлов; размеры — во внутренних единицах (мм)."
  (unless (and (integerp rings) (>= rings 2))
    (error "колец должно быть не меньше двух, а не ~S" rings))
  (unless (and (integerp ring-nodes) (>= ring-nodes 4) (zerop (mod ring-nodes 4)))
    (error "узлов на кольце должно быть кратно четырём, а не ~S" ring-nodes))
  (let* ((length (float length 0d0))
         (radius-max (float radius-max 0d0))
         (radius-end-min (float radius-end-min 0d0))
         (a (ellipse-semi-axis length radius-max radius-end-min))
         (spacing (/ length (1- rings)))
         (ring-s (make-dvec rings))
         (ring-radius (make-dvec rings))
         (ring-ds (make-dvec rings spacing))
         (coords (make-dvec (* 3 rings ring-nodes))))
    (dotimes (i rings)
      (let ((s (* i spacing)))
        (setf (aref ring-s i) s
              (aref ring-radius i) (ellipse-radius s length radius-max a))))
    (setf (aref ring-ds 0) (* 0.5d0 spacing)
          (aref ring-ds (1- rings)) (* 0.5d0 spacing))
    (dotimes (i rings)
      (dotimes (k ring-nodes)
        (let ((p (* 3 (node-index i k ring-nodes)))
              (r (aref ring-radius i))
              (theta (node-angle k ring-nodes)))
          (setf (aref coords p) (aref ring-s i)
                (aref coords (+ p 1)) (* r (cos theta))
                (aref coords (+ p 2)) (* r (sin theta))))))
    (let* ((ends (build-edges rings ring-nodes))
           (rest (loop for (family (ea eb)) on ends by #'cddr
                       append (list family (edge-lengths coords ea eb))))
           (compartment-ring (ivec-from (loop for i below (1- rings) collect i)))
           (volumes (make-dvec (1- rings))))
      (dotimes (c (1- rings))
        (setf (aref volumes c) (compartment-volume coords c ring-nodes)))
      (%make-tube :rings rings :ring-nodes ring-nodes :length length
                  :radius-max radius-max :radius-end-min radius-end-min :semi-axis a
                  :ring-s ring-s :ring-radius ring-radius :ring-ds ring-ds
                  :rest-coords coords :ends ends :rest rest
                  :compartment-ring compartment-ring :rest-volume volumes))))

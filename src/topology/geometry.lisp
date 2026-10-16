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

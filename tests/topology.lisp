;;;; tests/topology.lisp — трубка N × K.
;;;;
;;;; Условия готовности этапа 1: число элементов каждого семейства совпадает
;;;; с таблицей «Представление модели»; все объёмы покоя положительны, а
;;;; их сумма совпадает с объёмом многогранной трубки, посчитанным
;;;; независимым способом.

(in-package #:worm.tests)

(def-suite topology :in worm :description "worm.topology.")
(in-suite topology)

(defun base-tube (&rest args)
  (apply #'worm.topology:make-tube :rings 50 :ring-nodes 8 :length 1d0
         :radius-max 0.04d0 :radius-end-min 0.005d0 args))

(test family-sizes-match-architecture-table
  (let ((tb (base-tube)))
    (loop for (family expected) in '((:long-edge 392) (:circ-edge 400) (:diagonal 784)
                                     (:diametral 200) (:compartment 49) (:ring 50))
          do (is (= expected (worm.topology:family-size tb family))
                 "~(~A~): ~D, а по таблице ~D" family (worm.topology:family-size tb family) expected))
    (is (= (* 3 50 8) (length (worm.topology:tube-rest-coords tb))))))

(test family-sizes-follow-formulas
  (loop for (n k) in '((2 4) (7 8) (20 12) (33 16))
        for tb = (worm.topology:make-tube :rings n :ring-nodes k)
        do (is (= (* (1- n) k) (worm.topology:family-size tb :long-edge)))
           (is (= (* n k) (worm.topology:family-size tb :circ-edge)))
           (is (= (* 2 (1- n) k) (worm.topology:family-size tb :diagonal)))
           (is (= (/ (* n k) 2) (worm.topology:family-size tb :diametral)))))

(test edges-connect-the-right-nodes
  (let ((tb (base-tube)))
    (multiple-value-bind (a b) (worm.topology:family-ends tb :long-edge)
      (is (= 8 (- (aref b 0) (aref a 0))) "продольное ребро — к тому же узлу следующего кольца")
      (is (= (worm.topology:long-edge-index 10 3 8) (+ (* 10 8) 3))))
    (multiple-value-bind (a b) (worm.topology:family-ends tb :circ-edge)
      (is (= 7 (aref a 7))) (is (= 0 (aref b 7)) "окружные рёбра замыкают кольцо"))
    (multiple-value-bind (a b) (worm.topology:family-ends tb :diametral)
      (is (= 4 (- (aref b 0) (aref a 0))) "диаметральное ребро — к противоположному узлу"))
    (dolist (family worm.topology:+families+)
      (multiple-value-bind (a b) (worm.topology:family-ends tb family)
        (is (every (lambda (x) (< x 400)) a))
        (is (every (lambda (x) (< x 400)) b))
        (is (every #'/= a b) "~(~A~): ребро из узла в себя" family)
        (is (every #'plusp (worm.topology:family-rest tb family)))))))

(test rest-lengths-match-geometry
  (let* ((tb (base-tube))
         (radius (worm.topology:tube-ring-radius tb)))
    (let ((diam (worm.topology:family-rest tb :diametral)))
      (dotimes (i 50)
        (dotimes (k 4)
          (is (close-to (* 2 (aref radius i)) (aref diam (+ (* i 4) k)) 1d-12)))))
    (let ((circ (worm.topology:family-rest tb :circ-edge)))
      (is (close-to (* 2 (aref radius 25) (sin (/ pi 8))) (aref circ (* 25 8)) 1d-12)
          "сторона правильного восьмиугольника"))))

(test profile-is-the-ellipse-of-the-parameters
  (let* ((tb (base-tube))
         (radius (worm.topology:tube-ring-radius tb)))
    (is (close-to 0.005d0 (aref radius 0) 1d-12) "радиус крайнего кольца — radius-end-min")
    (is (close-to 0.005d0 (aref radius 49) 1d-12))
    (is (< (abs (- 0.04d0 (reduce #'max radius))) 1d-4))
    (is (< 0.5d0 (worm.topology:tube-semi-axis tb) 0.505d0) "ε порядка нескольких микрометров")
    (is (close-to 1d0 (reduce #'+ (worm.topology:tube-ring-ds tb)) 1d-12) "участки колец покрывают тело")))

(test rest-volumes-are-positive-and-sum-to-the-frustums
  (let* ((tb (base-tube))
         (volumes (worm.topology:tube-rest-volume tb))
         (radius (worm.topology:tube-ring-radius tb))
         (s (worm.topology:tube-ring-s tb))
         (independent (loop for c below 49
                            sum (worm.topology:frustum-volume
                                 (aref radius c) (aref radius (1+ c))
                                 (- (aref s (1+ c)) (aref s c)) 8))))
    (is (every #'plusp volumes))
    (is (close-to independent (reduce #'+ volumes) 1d-12)
        "сумма ~,12F, по формуле усечённых пирамид ~,12F" (reduce #'+ volumes) independent)
    (dotimes (c 49)
      (is (close-to (worm.topology:frustum-volume (aref radius c) (aref radius (1+ c))
                                                  (- (aref s (1+ c)) (aref s c)) 8)
                    (aref volumes c) 1d-12)))
    ;; и порядок величины: объём восьмигранной трубки чуть меньше эллипсоида вращения
    (let ((ellipsoid (* 4/3 pi 0.5d0 0.04d0 0.04d0)))
      (is (< (* 0.8 ellipsoid) (reduce #'+ volumes) ellipsoid)))))

(defun rigid-motion (coords angle-x angle-z shift)
  "Повернуть координаты вокруг x и z и сдвинуть — объём не должен измениться."
  (let ((out (copy-seq coords)))
    (loop for p from 0 below (length coords) by 3
          do (let* ((x (aref coords p)) (y (aref coords (+ p 1))) (z (aref coords (+ p 2)))
                    (y1 (- (* y (cos angle-x)) (* z (sin angle-x))))
                    (z1 (+ (* y (sin angle-x)) (* z (cos angle-x))))
                    (x2 (- (* x (cos angle-z)) (* y1 (sin angle-z))))
                    (y2 (+ (* x (sin angle-z)) (* y1 (cos angle-z)))))
               (setf (aref out p) (+ x2 (first shift))
                     (aref out (+ p 1)) (+ y2 (second shift))
                     (aref out (+ p 2)) (+ z1 (third shift)))))
    out))

(test volume-is-a-closed-surface-invariant
  ;; Объём замкнутого многогранника не зависит от положения начала
  ;; координат; если бы поверхность отсека не была замкнута или грани были
  ;; ориентированы непоследовательно, сдвиг изменил бы сумму.
  (let* ((tb (worm.topology:make-tube :rings 6 :ring-nodes 8))
         (state (make-random-state nil))
         (coords (copy-seq (worm.topology:tube-rest-coords tb))))
    (dotimes (p (length coords))
      (incf (aref coords p) (* 0.004d0 (- (random 1d0 state) 0.5d0))))
    (let ((moved (rigid-motion coords 0.7d0 -1.3d0 '(5d0 -3d0 2d0))))
      (dotimes (c 5)
        (let ((v (worm.topology:compartment-volume coords c 8)))
          (is (plusp v))
          (is (close-to v (worm.topology:compartment-volume moved c 8) 1d-9)))))))

(test node-numbering-and-quadrants
  (is (equal '(0 1) (worm.topology:quadrant-nodes :dorsal-left 8)))
  (is (equal '(2 3) (worm.topology:quadrant-nodes :ventral-left 8)))
  (is (equal '(4 5) (worm.topology:quadrant-nodes :ventral-right 8)))
  (is (equal '(6 7) (worm.topology:quadrant-nodes :dorsal-right 8)))
  (is (equal '(:medial :lateral :lateral :medial :medial :lateral :lateral :medial)
             (loop for k below 8 collect (worm.topology:node-row k 8))))
  (is (close-to (/ pi 8) (worm.topology:node-angle 0 8) 1d-15) "22,5° от дорсальной линии")
  (let* ((tb (base-tube)) (c (worm.topology:tube-rest-coords tb)))
    (is (plusp (aref c (+ (* 3 (+ 80 0)) 1))) "узел 0 — дорсальный (y > 0)")
    (is (plusp (aref c (+ (* 3 (+ 80 0)) 2))) "и левый (z > 0)")
    (is (minusp (aref c (+ (* 3 (+ 80 4)) 1))) "узел 4 — вентральный")
    (is (minusp (aref c (+ (* 3 (+ 80 4)) 2))) "и правый")))

(test bad-topology-is-refused
  (signals error (worm.topology:make-tube :rings 1))
  (signals error (worm.topology:make-tube :ring-nodes 6))
  (signals error (worm.topology:make-tube :radius-end-min 0.05d0)))

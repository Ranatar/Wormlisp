;;;; tests/anatomy.lisp — мышцы и их привязка к рёбрам.
;;;;
;;;; Условие готовности этапа 1: анатомия даёт 95 мышц, каждая привязана к
;;;; двум рядам рёбер своего квадранта.

(in-package #:worm.tests)

(def-suite anatomy :in worm :description "worm.anatomy.")
(in-suite anatomy)

(defun bind-to-tube (anatomy tube)
  (worm.anatomy:bind-muscles (worm.anatomy:anatomy-muscles anatomy)
                             (worm.topology:tube-ring-fractions tube)
                             (lambda (q i) (worm.topology:quadrant-long-edges tube q i))))

(test muscles-file-gives-95-muscles
  (let* ((a (worm.anatomy:load-anatomy))
         (muscles (worm.anatomy:anatomy-muscles a)))
    (is (= 95 (length muscles)))
    (loop for (q n) on worm.anatomy:+muscles-per-quadrant+ by #'cddr
          do (is (= n (count q muscles :key #'worm.anatomy:muscle-quadrant))))
    (is (= 48 (count-if (lambda (m) (member (worm.anatomy:muscle-quadrant m) '(:dorsal-left :dorsal-right)))
                        muscles)))
    (is (worm.anatomy:find-muscle a "mvl23"))
    (is (null (worm.anatomy:find-muscle a "mvl24")) "в VL 23 клетки")))

(test muscles-file-matches-its-rule
  (let* ((a (worm.anatomy:load-anatomy))
         (layout (worm.anatomy:anatomy-layout a))
         (expected (worm.anatomy:generate-muscles layout)))
    (is (not (null layout)))
    (loop for m across (worm.anatomy:anatomy-muscles a)
          for e in expected
          do (is (eq (worm.anatomy:muscle-name e) (worm.anatomy:muscle-name m)))
             (is (< (abs (- (worm.anatomy:muscle-from e) (worm.anatomy:muscle-from m))) 1d-6)
                 "~(~A~) разошлась с правилом; перезапустите scripts/make-muscles.lisp"
                 (worm.anatomy:muscle-name m))
             (is (< (abs (- (worm.anatomy:muscle-peak e) (worm.anatomy:muscle-peak m))) 1d-6))
             (is (< (abs (- (worm.anatomy:muscle-to e) (worm.anatomy:muscle-to m))) 1d-6)))))

(test layout-follows-wormatlas
  (let* ((layout (worm.anatomy:anatomy-layout (worm.anatomy:load-anatomy)))
         (muscles (worm.anatomy:generate-muscles layout))
         (vulva (worm.anatomy:muscle-layout-vulva layout)))
    (dolist (name '(:mvl16 :mvr16))
      (is (= vulva (worm.anatomy:muscle-peak (find name muscles :key #'worm.anatomy:muscle-name)))
          "клетка 16 вентральных квадрантов — на уровне вульвы"))
    (is (= 64 (count-if (lambda (m) (<= (worm.anatomy:muscle-peak m) vulva)) muscles))
        "по 16 клеток каждого квадранта впереди вульвы — две трети")
    (is (every (lambda (m) (eq (worm.anatomy:muscle-row m)
                               (if (oddp (worm.anatomy:muscle-index m)) :medial :lateral)))
               muscles))))

(defun muscles-from (forms)
  (coerce (mapcar (lambda (f) (apply #'worm.anatomy:make-muscle f)) forms) 'simple-vector))

(test validation-catches-broken-anatomy
  (let ((good (worm.anatomy:generate-muscles (worm.anatomy:anatomy-layout (worm.anatomy:load-anatomy)))))
    (finishes (worm.anatomy:validate-muscles (coerce good 'simple-vector)))
    (signals worm.anatomy:anatomy-invalid
      (worm.anatomy:validate-muscles (coerce (rest good) 'simple-vector)))
    (signals worm.anatomy:anatomy-invalid
      (worm.anatomy:validate-muscles (coerce (cons (first good) good) 'simple-vector)))
    (signals worm.anatomy:anatomy-invalid
      (worm.anatomy:validate-muscles
       (coerce (cons (worm.anatomy:make-muscle :name :mdl01 :quadrant :dorsal-left :row :lateral
                                               :index 1 :from 0.02d0 :peak 0.05d0 :to 0.09d0)
                     (rest good))
               'simple-vector))
      "нечётная клетка в латеральном ряду")
    (signals worm.anatomy:anatomy-invalid
      (worm.anatomy:validate-muscles
       (coerce (cons (worm.anatomy:make-muscle :name :mdl01 :quadrant :dorsal-left :row :medial
                                               :index 1 :from 0.3d0 :peak 0.2d0 :to 0.4d0)
                     (rest good))
               'simple-vector)))))

(test broken-file-offers-restarts
  (let ((path (merge-pathnames (format nil "worm-muscles-~D.sexp" (random 1000000))
                               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output :if-exists :supersede)
             (write-string "(muscle mdl01 :quadrant :dorsal-middle :row :medial :index 1 :from 0 :peak 0.1 :to 0.2)" out))
           (signals worm.anatomy:anatomy-invalid (worm.anatomy:load-anatomy :muscles-path path))
           (let ((fallback (worm.anatomy:load-anatomy)))
             (is (eq fallback
                     (handler-bind ((worm.anatomy:anatomy-invalid
                                      (lambda (c) (declare (ignore c)) (use-value fallback))))
                       (worm.anatomy:load-anatomy :muscles-path path)))))
           (with-open-file (out path :direction :output :if-exists :supersede)
             (write-string "(muscle mdl01 :quadrant :dorsal-left #.(error \"x\"))" out))
           (signals worm.anatomy:anatomy-invalid (worm.anatomy:load-anatomy :muscles-path path)))
      (delete-file path))))

(test every-muscle-is-bound-to-two-rows-of-its-quadrant
  (let* ((a (worm.anatomy:load-anatomy))
         (tube (base-tube))
         (b (bind-to-tube a tube))
         (muscles (worm.anatomy:muscle-binding-muscles b)))
    (is (= 95 (length muscles)))
    (loop for m across muscles
          for j from 0
          for edges = (worm.anatomy:muscle-edges b j)
          for nodes = (remove-duplicates (mapcar (lambda (e) (mod (car e) 8)) edges))
          do (is (plusp (length edges)) "~(~A~) ни к чему не привязана" (worm.anatomy:muscle-name m))
             (is (equal (worm.topology:quadrant-nodes (worm.anatomy:muscle-quadrant m) 8)
                        (sort nodes #'<))
                 "~(~A~) привязана к узлам ~S, а не к двум рядам своего квадранта"
                 (worm.anatomy:muscle-name m) nodes)
             ;; два ряда рёбер получают одинаковые доли
             (let ((by-row (loop for k in (worm.topology:quadrant-nodes (worm.anatomy:muscle-quadrant m) 8)
                                 collect (loop for (e . w) in edges when (= k (mod e 8)) sum w))))
               (is (close-to (first by-row) (second by-row) 1d-12))))))

(test weights-cover-the-body-evenly
  ;; Профили соседних клеток перекрываются так, что между вершинами первой
  ;; и последней клетки квадранта их сумма равна единице: на каждое
  ;; продольное ребро квадранта приходится ровно половина.
  (let* ((a (worm.anatomy:load-anatomy))
         (tube (base-tube))
         (b (bind-to-tube a tube))
         (muscles (worm.anatomy:muscle-binding-muscles b))
         (fractions (worm.topology:tube-ring-fractions tube))
         (total (make-hash-table)))
    (loop for j below (length muscles)
          do (loop for (e . w) in (worm.anatomy:muscle-edges b j)
                   do (incf (gethash e total 0d0) w)))
    (dolist (q worm.anatomy:+muscle-quadrants+)
      (let* ((cells (remove q (coerce muscles 'list) :key #'worm.anatomy:muscle-quadrant :test-not #'eq))
             (first-peak (reduce #'min cells :key #'worm.anatomy:muscle-peak))
             (last-peak (reduce #'max cells :key #'worm.anatomy:muscle-peak)))
        (dotimes (i 49)
          (when (and (>= (aref fractions i) first-peak) (<= (aref fractions (1+ i)) last-peak))
            (dolist (e (worm.topology:quadrant-long-edges tube q i))
              (is (close-to 0.5d0 (gethash e total 0d0) 1d-12)
                  "~(~A~), участок ~D: сумма весов ~F" q i (gethash e total 0d0)))))))
    ;; полный вес клетки — площадь её профиля в долях участка
    (let ((ds (- (aref fractions 1) (aref fractions 0))))
      (loop for m across muscles for j from 0
            do (is (close-to (* 0.5d0 (- (worm.anatomy:muscle-to m) (worm.anatomy:muscle-from m)))
                             (* ds (loop for (nil . w) in (worm.anatomy:muscle-edges b j) sum w))
                             1d-10))))))

(test binding-does-not-depend-on-ring-count
  ;; Файл анатомии хранит доли длины, поэтому при другом N привязка
  ;; меняется, а площадь профиля каждой клетки — нет.
  (let ((a (worm.anatomy:load-anatomy)))
    (dolist (n '(30 100))
      (let* ((tube (worm.topology:make-tube :rings n :ring-nodes 8))
             (b (bind-to-tube a tube))
             (ds (/ 1d0 (1- n))))
        (loop for m across (worm.anatomy:muscle-binding-muscles b) for j from 0
              do (is (close-to (* 0.5d0 (- (worm.anatomy:muscle-to m) (worm.anatomy:muscle-from m)))
                               (* ds (loop for (nil . w) in (worm.anatomy:muscle-edges b j) sum w))
                               1d-10)))))))

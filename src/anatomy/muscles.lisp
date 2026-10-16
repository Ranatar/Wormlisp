;;;; src/anatomy/muscles.lisp — мышцы стенки тела и правило их раскладки.
;;;;
;;;; По WormAtlas (Altun, Hall 2009; docs/parameters.md, «Геометрия тела»):
;;;; 95 клеток в четырёх квадрантах — DL, DR, VR по 24, VL — 23. В каждом
;;;; квадранте клетки пронумерованы спереди назад; нечётные — медиальный
;;;; ряд, чётные — латеральный. Клетки 16 обоих вентральных квадрантов
;;;; лежат на уровне вульвы, две трети клеток — впереди неё.
;;;;
;;;; Точных координат клеток источник не даёт, поэтому они строятся по
;;;; правилу (степень О). Вдоль квадранта ставятся опорные точки
;;;; t₀ = начало мышечных рядов, t₁…t₁₆ — равномерно до вульвы (t₁₆ = вульва),
;;;; t₁₇…t_n — равномерно за ней, t_{n+1} = конец рядов. Клетка k занимает
;;;; участок (t_{k−1}, t_{k+1}) с вершиной в t_k, а её сила вдоль клетки
;;;; убывает к концам линейно, как сечение веретена. Соседние клетки
;;;; перекрываются наполовину, как на схеме рядов, а сумма профилей всех
;;;; клеток квадранта между t₁ и t_n равна единице.

(in-package #:worm.anatomy)

(defparameter +muscle-quadrants+ '(:dorsal-left :ventral-left :ventral-right :dorsal-right)
  "Квадранты в порядке обхода кольца (как в worm.topology).")

(defparameter +muscles-per-quadrant+
  '(:dorsal-left 24 :ventral-left 23 :ventral-right 24 :dorsal-right 24))

(defparameter +cells-anterior-to-vulva+ 16
  "Клетки 1…16 каждого квадранта лежат впереди вульвы или на её уровне.")

(defparameter +quadrant-prefix+
  '(:dorsal-left "mdl" :ventral-left "mvl" :ventral-right "mvr" :dorsal-right "mdr"))

(defstruct (muscle (:copier nil))
  (name nil :type keyword :read-only t)
  (quadrant nil :type keyword :read-only t)
  (row nil :type (member :medial :lateral) :read-only t)
  (index 0 :type (integer 1 24) :read-only t)
  (from 0d0 :type double-float :read-only t)  ; Передний конец, доля L.
  (peak 0d0 :type double-float :read-only t)  ; Середина веретена, доля L.
  (to 0d0 :type double-float :read-only t))  ; Задний конец, доля L.

(defmethod print-object ((m muscle) stream)
  (print-unreadable-object (m stream :type t)
    (format stream "~(~A~) ~,3F…~,3F" (muscle-name m) (muscle-from m) (muscle-to m))))

(defstruct (muscle-layout (:copier nil))
  (head-start 0.02d0 :type double-float :read-only t)
  (vulva 0.57d0 :type double-float :read-only t)
  (tail-end 0.95d0 :type double-float :read-only t)
  (source "" :read-only t))

(defun muscle-name-for (quadrant index)
  (intern (string-upcase (format nil "~A~2,'0D" (getf +quadrant-prefix+ quadrant) index))
          :keyword))

(defun quadrant-knots (count layout)
  "Опорные точки t₀…t_{n+1} квадранта из COUNT клеток."
  (let* ((head (muscle-layout-head-start layout))
         (vulva (muscle-layout-vulva layout))
         (tail (muscle-layout-tail-end layout))
         (front +cells-anterior-to-vulva+)
         (back (- count front))
         (knots (make-dvec (+ count 2))))
    (setf (aref knots 0) head)
    (loop for k from 1 to front
          do (setf (aref knots k) (+ head (* k (/ (- vulva head) front)))))
    (loop for j from 1 to (1+ back)
          do (setf (aref knots (+ front j)) (+ vulva (* j (/ (- tail vulva) (1+ back))))))
    knots))

(defun generate-muscles (layout)
  "Все 95 мышц по правилу раскладки LAYOUT, в порядке квадрантов и номеров."
  (unless (< 0d0 (muscle-layout-head-start layout) (muscle-layout-vulva layout)
             (muscle-layout-tail-end layout) 1.000000001d0)
    (error 'anatomy-invalid
           :message (format nil "раскладка мышц: нужно 0 < начало < вульва < конец ≤ 1, а не ~,3F, ~,3F, ~,3F"
                            (muscle-layout-head-start layout) (muscle-layout-vulva layout)
                            (muscle-layout-tail-end layout))))
  (loop for quadrant in +muscle-quadrants+
        for count = (getf +muscles-per-quadrant+ quadrant)
        for knots = (quadrant-knots count layout)
        append (loop for k from 1 to count
                     collect (make-muscle :name (muscle-name-for quadrant k)
                                          :quadrant quadrant
                                          :row (if (oddp k) :medial :lateral)
                                          :index k
                                          :from (aref knots (1- k))
                                          :peak (aref knots k)
                                          :to (aref knots (1+ k))))))

(defun muscle-profile (muscle s)
  "Относительная сила клетки в точке S (доля L): 1 в вершине, линейно до 0 на концах."
  (let ((from (muscle-from muscle)) (peak (muscle-peak muscle)) (to (muscle-to muscle)))
    (cond ((or (<= s from) (>= s to)) 0d0)
          ((<= s peak) (/ (- s from) (- peak from)))
          (t (/ (- to s) (- to peak))))))

;;; Нейроны и коннектом (этап 12): чтение и запись — в connectome.lisp.

(defparameter +transmitters+ '(:acetylcholine :gaba))

(defstruct (neuron (:copier nil))
  (name nil :type keyword :read-only t)
  (class nil :type keyword :read-only t)
  (transmitter :acetylcholine :read-only t)
  (position nil :read-only t)
  (nmj 0 :read-only t))

(defstruct (connectome (:copier nil))
  (neurons #() :type simple-vector :read-only t)
  (chemical #() :type simple-vector :read-only t)   ; #((пре пост контакты) …), номера нейронов
  (gap #() :type simple-vector :read-only t)        ; #((a b контакты) …), a < b
  (nmj #() :type simple-vector :read-only t)        ; #((нейрон мышца контакты) …), номер мышцы
  (source nil :read-only t))                        ; plist формы connectome-source

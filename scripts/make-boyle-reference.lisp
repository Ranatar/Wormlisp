;;;; scripts/make-boyle-reference.lisp — прогоны исходной модели Бойла в WCON
;;;; (план, этап 15).
;;;;
;;;; Запуск из корня репозитория после scripts/boyle-wormsim.sh <каталог>:
;;;;   WORM_BOYLE_RUNS=<каталог> sbcl --non-interactive --no-userinit \
;;;;        --load scripts/init.lisp --load scripts/make-boyle-reference.lisp
;;;;
;;;; simdata.csv WormSim — строка на кадр (25 кадров/с): время, затем x, y, φ
;;;; каждого из 49 стержней в метрах; стержень 0 — голова. В WCON идут x и y
;;;; в мм с точностью 0,1 мкм, голова — первая точка.

(asdf:load-system "worm/io")

(defpackage #:worm.make-boyle (:use #:cl))
(in-package #:worm.make-boyle)

(defparameter *media* '("0" "0.001" "0.01" "0.1" "1"))
(defparameter *commit* "c2455768a7c01b9c30b7fd3329121fbe3cec748f")

(defun round-to (x digits) (let ((k (expt 10 digits))) (/ (fround (* x k)) k)))

(defun parse-row (line)
  (let ((*read-default-float-format* 'double-float) (*read-eval* nil))
    (mapcar (lambda (s) (float (read-from-string s) 0d0))
            (uiop:split-string (remove #\Space line) :separator ","))))

(defun boyle-wcon (path medium)
  (let ((ts '()) (xs '()) (ys '()))
    (with-open-file (in path)
      (loop for line = (read-line in nil) while line
            for f = (parse-row line)
            for v = (coerce (rest f) 'vector)
            for n = (floor (length v) 3)
            do (push (round-to (first f) 3) ts)
               (push (map 'vector (lambda (i) (round-to (* 1000 (aref v (* 3 i))) 4)) (loop for i below n collect i)) xs)
               (push (map 'vector (lambda (i) (round-to (* 1000 (aref v (1+ (* 3 i)))) 4)) (loop for i below n collect i)) ys)))
    (worm.io:json-object
     "units" (worm.io:json-object "t" "s" "x" "mm" "y" "mm")
     "metadata" (worm.io:json-object
                 "who" "Boyle, Berri, Cohen 2012 model (WormSim)"
                 "software" (worm.io:json-object "name" "WormSim (github.com/openworm/CelegansNeuromechanicalGaitModulation)"
                                                 "version" *commit*))
     "@Wormlisp" (worm.io:json-object
                  "source" "Boyle J.H., Berri S., Cohen N. (2012) Gait modulation in C. elegans: an integrated neuromechanical model. Front. Comput. Neurosci. 6:10; WormSim code, github.com/openworm/CelegansNeuromechanicalGaitModulation"
                  "medium" (let ((*read-default-float-format* 'double-float)) (float (read-from-string medium) 0d0))
                  "note" "MEDIUM interpolates drag linearly: 0 water, 1 agar; DURATION and MEDIUM read from the environment, worm.cc otherwise unchanged; scripts/boyle-wormsim.sh")
     "data" (vector (worm.io:json-object "id" "1" "head" "L"
                                         "t" (coerce (nreverse ts) 'vector)
                                         "x" (coerce (nreverse xs) 'vector)
                                         "y" (coerce (nreverse ys) 'vector))))))

(let ((runs (or (uiop:getenv "WORM_BOYLE_RUNS") (error "WORM_BOYLE_RUNS не задан")))
      (root (worm.core:project-root)))
  (dolist (m *media*)
    (let ((csv (merge-pathnames (format nil "m~A/simdata.csv" m) (uiop:ensure-directory-pathname runs)))
          (out (merge-pathnames (format nil "reference/boyle2012-wormsim/medium-~A.wcon" m) root)))
      (with-open-file (s out :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8)
        (com.inuoe.jzon:stringify (boyle-wcon csv m) :stream s)
        (terpri s))
      (format t "~&~A~%" out))))

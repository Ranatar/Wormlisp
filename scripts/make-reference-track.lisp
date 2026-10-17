;;;; scripts/make-reference-track.lisp — отрывок трека живого червя для сверки
;;;; метрик походки (этап 14).
;;;;
;;;; Запуск из корня репозитория, путь к полному треку — в WORM_TRACK_SOURCE:
;;;;   WORM_TRACK_SOURCE=".../N2 on food L_2009_09_15__11_01_01___8___1.wcon" \
;;;;   sbcl --dynamic-space-size 4096 --non-interactive --no-userinit \
;;;;        --load scripts/init.lisp --load scripts/make-reference-track.lisp
;;;;
;;;; Источник — Open Worm Movement Database (Javer et al. 2018, Nature Methods
;;;; 15:645; Zenodo, CC BY 4.0), запись лаборатории Шафера; копия — пример в
;;;; github.com/openworm/WCONViewer (examples/, архив .wcon.zip). Полный трек —
;;;; 30 мин, 25 кадров/с, 49 точек средней линии, 110 МБ; в репозиторий идёт
;;;; отрывок:
;;;;   - отрезки, в которых нет пропусков длиннее *max-gap* кадров (0,24 с —
;;;;     столько метрики интерполируют, как у Yemini et al. 2013), длиной не
;;;;     меньше *min-bout* секунд, по порядку времени, пока их сумма не
;;;;     превысит *total*;
;;;;   - каждый *every*-й кадр; пропуски внутри отрезков остаются null;
;;;;   - координаты в мм с *digits* знаками.
;;;; Отбор движения вперёд и сглаживание — дело метрик, а не отрывка.

(asdf:load-system "worm/io")

(defpackage #:worm.make-track (:use #:cl))
(in-package #:worm.make-track)

(defparameter *max-gap* 6)
(defparameter *min-bout* 30d0)
(defparameter *total* 300d0)
(defparameter *every* 2)
(defparameter *digits* 3)
(defparameter *target* "reference/schafer-n2-agar/track.wcon")

(defun valid-frame-p (xs)
  (and (vectorp xs) (plusp (length xs)) (every #'realp xs)))

(defun unit-scale (unit)
  "Множитель единицы длины UNIT к миллиметрам."
  (cond ((member unit '("mm" "millimeters" "millimetres") :test #'string-equal) 1d0)
        ((member unit '("um" "micrometers" "micrometres" "microns") :test #'string-equal) 1d-3)
        (t (error "единица длины ~S неизвестна" unit))))

(defun round-to (x digits) (let ((k (expt 10 digits))) (/ (fround (* x k)) k)))

(let* ((root (worm.core:project-root))
       (source (or (uiop:getenv "WORM_TRACK_SOURCE") (error "WORM_TRACK_SOURCE не задан")))
       (w (worm.io:read-json-file source))
       (units (gethash "units" w))
       (scale (unit-scale (gethash "x" units)))
       (record (aref (gethash "data" w) 0))
       (ts (gethash "t" record)) (xs (gethash "x" record)) (ys (gethash "y" record))
       (n (length ts))
       (ok (map 'vector #'valid-frame-p xs))
       (bouts '()))
  ;; отрезки без длинных пропусков
  (let ((start nil) (gap 0) (last-ok nil))
    (flet ((close-bout ()
             (when (and start last-ok (>= (- (aref ts last-ok) (aref ts start)) *min-bout*))
               (push (cons start (1+ last-ok)) bouts))
             (setf start nil last-ok nil gap 0)))
      (dotimes (k n)
        (cond ((aref ok k) (unless start (setf start k)) (setf last-ok k gap 0))
              (start (incf gap) (when (> gap *max-gap*) (close-bout)))))
      (close-bout)))
  (setf bouts (nreverse bouts))
  (let* ((picked (loop with sum = 0d0
                       for (a . b) in bouts
                       while (< sum *total*)
                       collect (cons a b)
                       do (incf sum (- (aref ts (1- b)) (aref ts a)))))
         (frames (loop for (a . b) in picked
                       append (loop for k from a below b by *every* collect k)))
         (point (lambda (v) (if (realp v) (round-to (* scale v) *digits*) 'null)))
         (coords (lambda (src k)
                   (let ((v (aref src k)))
                     (if (aref ok k) (map 'vector point v) (make-array (length (aref xs (first frames))) :initial-element 'null)))))
         (meta (gethash "metadata" w))
         (out (worm.io:json-object
               "units" (worm.io:json-object "t" "s" "x" "mm" "y" "mm")
               "metadata" meta
               "@Wormlisp" (worm.io:json-object
                            "source" "Open Worm Movement Database (Javer et al. 2018, Nature Methods 15:645), CC BY 4.0; copy: github.com/openworm/WCONViewer examples/N2 on food L_2009_09_15__11_01_01___8___1.wcon.zip"
                            "sourceMd5" (worm.platform:md5-hex (uiop:read-file-string source))
                            "selection" (format nil "bouts without gaps over ~D frames, at least ~,1F s, in time order up to ~,1F s; every ~:R frame; mm, ~D digits; scripts/make-reference-track.lisp"
                                                *max-gap* *min-bout* *total* *every* *digits*)
                            "bouts" (map 'vector (lambda (p) (vector (aref ts (car p)) (aref ts (1- (cdr p))))) picked))
               "data" (vector
                       (worm.io:json-object
                        "id" (gethash "id" record)
                        "head" (gethash "head" record)
                        "ventral" (gethash "ventral" record)
                        "t" (map 'vector (lambda (k) (round-to (aref ts k) 3)) frames)
                        "x" (map 'vector (lambda (k) (funcall coords xs k)) frames)
                        "y" (map 'vector (lambda (k) (funcall coords ys k)) frames))))))
    (let ((path (merge-pathnames *target* root)))
      (ensure-directories-exist path)
      (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
        (com.inuoe.jzon:stringify out :stream s)
        (terpri s))
      (format t "~&Отрезков ~D, кадров ~D, ~,1F с; ~A~%" (length picked) (length frames)
              (loop for (a . b) in picked sum (- (aref ts (1- b)) (aref ts a))) path))))

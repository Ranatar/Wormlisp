;;;; tests/analysis.lisp — метрики походки на синтетической бегущей волне.
;;;;
;;;; Средняя линия задана кривизной κ(s, t) = A·sin(2π(s/λ − f·t)) — волна
;;;; от головы (s = 0) к хвосту; тело как целое сдвигается к голове (−x) со
;;;; скоростью v. Кольца трубки ставятся поперёк средней линии, дорсальная
;;;; сторона — по нормали, в которую изгиб положителен. Метрики обязаны
;;;; вернуть заданные f, λ и v.

(in-package #:worm.tests)

(def-suite analysis :in worm :description "Метрики походки.")
(in-suite analysis)

(defun synthetic-wave-coords (tube time &key amplitude wavelength frequency speed)
  (let* ((rings (worm.topology:tube-rings tube))
         (kk (worm.topology:tube-ring-nodes tube))
         (len (worm.topology:tube-length tube))
         (s (worm.topology:tube-ring-s tube))
         (rest (worm.topology:tube-rest-coords tube))
         (theta (worm.core:make-dvec rings))
         (cx (worm.core:make-dvec rings)) (cy (worm.core:make-dvec rings))
         (out (worm.core:make-dvec (length rest))))
    (flet ((kappa (si) (* amplitude (sin (* 2 pi (- (/ si len wavelength) (* frequency time)))))))
      ;; угол касательной — интеграл кривизны (трапеции); средний угол убран,
      ;; чтобы тело в среднем лежало вдоль +x (от головы к хвосту)
      (loop for i from 1 below rings
            do (setf (aref theta i) (+ (aref theta (1- i))
                                       (* 0.5d0 (- (aref s i) (aref s (1- i)))
                                          (+ (kappa (aref s i)) (kappa (aref s (1- i))))))))
      (let ((mean (/ (reduce #'+ theta) rings)))
        (dotimes (i rings) (decf (aref theta i) mean)))
      (loop for i from 1 below rings
            for ds = (- (aref s i) (aref s (1- i)))
            for th = (* 0.5d0 (+ (aref theta i) (aref theta (1- i))))
            do (setf (aref cx i) (+ (aref cx (1- i)) (* ds (cos th)))
                     (aref cy i) (+ (aref cy (1- i)) (* ds (sin th)))))
      (let ((mx (/ (reduce #'+ cx) rings)) (my (/ (reduce #'+ cy) rings)))
        (dotimes (i rings out)
          (let ((nx (- (sin (aref theta i)))) (ny (cos (aref theta i))))
            (dotimes (k kk)
              (let* ((p (* 3 (+ (* i kk) k)))
                     (oy (aref rest (+ p 1))) (oz (aref rest (+ p 2))))
                (setf (aref out p) (+ (- (aref cx i) mx) (* nx oy) (- (* speed time)))
                      (aref out (+ p 1)) (+ (- (aref cy i) my) (* ny oy))
                      (aref out (+ p 2)) oz)))))))))

(defun write-synthetic-run (id &key (amplitude 6d0) (wavelength 0.8d0) (frequency 1.5d0)
                                    (speed 0.2d0) (duration 4d0) (dt 0.01d0))
  (let* ((tube (worm.topology:make-tube :rings 50 :ring-nodes 8))
         (dir (worm.io:make-run-directory
               :id id :root (merge-pathnames "cache/test-io/runs/" (worm.core:project-root))))
         (writer (worm.io:open-frames-writer (worm.io:run-file dir "frames.bin")
                                             (worm.topology:tube-node-count tube) 1))
         (acts (worm.core:make-dvec 1)))
    (loop for k from 0 to (round duration dt)
          for time = (* k dt)
          do (worm.io:write-frame writer k time
                                  (synthetic-wave-coords tube time :amplitude amplitude
                                                                   :wavelength wavelength
                                                                   :frequency frequency :speed speed)
                                  acts))
    (worm.io:close-frames-writer writer)
    (worm.io:write-json-file (worm.io:run-file dir "meta.json")
                             (worm.io:json-object
                              "topology" (worm.io:json-object "rings" 50 "ringNodes" 8
                                                              "length" (worm.topology:tube-length tube))))
    dir))

(test synthetic-wave-metrics
  (let* ((run (worm.analysis:load-run (write-synthetic-run "synthetic-wave")))
         (m (worm.analysis:gait-metrics run))
         (curv (worm.analysis:curvature-series run)))
    (is (< (abs (- (getf m :frequency) 1.5d0)) 0.01d0) "частота ~,4F Гц" (getf m :frequency))
    (is (< (abs (- (getf m :wavelength) 0.8d0)) 0.02d0) "длина волны ~,4F L" (getf m :wavelength))
    (is (< (abs (- (getf m :speed) 0.2d0)) 0.01d0) "скорость вперёд ~,4F мм/с" (getf m :speed))
    (is (< (abs (- (getf m :amplitude) 6d0)) 0.3d0) "размах |κ|·L ~,3F" (getf m :amplitude))
    ;; знак кривизны: в момент t = 0 на s = λ/4 изгиб дорсальный (κ > 0)
    (let ((i (round (* 0.2d0 49))))
      (is (plusp (aref curv 0 i)) "кривизна дорсального изгиба положительна: ~,3F" (aref curv 0 i)))))

(test windows-and-csv
  (let* ((run (worm.analysis:load-run (write-synthetic-run "synthetic-windows" :frequency 2d0)))
         (windows (worm.analysis:metrics-windows run 1d0))
         (path (merge-pathnames "cache/test-io/metrics.csv" (worm.core:project-root))))
    (is (= 4 (length windows)))
    (dolist (w windows)
      (is (< (abs (- (getf (cddr w) :frequency) 2d0)) 0.02d0) "окно ~,2F–~,2F: ~,4F Гц"
          (first w) (second w) (getf (cddr w) :frequency)))
    (worm.analysis:write-metrics-csv path windows)
    (with-open-file (in path)
      (is (string= "t_start_s,t_end_s,speed_mm_s,frequency_hz,wavelength_L,curvature_amplitude_kL"
                   (read-line in)))
      (is (= 4 (loop while (read-line in nil) count t))))))

(test still-body-has-no-frequency
  ;; Неподвижное прямое тело: переходов через ноль нет — частота и длина
  ;; волны не определены, скорость ноль. Шум около нуля не даёт периодов.
  (let* ((run (worm.analysis:load-run (write-synthetic-run "synthetic-still" :amplitude 0d0 :speed 0d0
                                                                             :duration 1d0)))
         (m (worm.analysis:gait-metrics run)))
    (is (null (getf m :frequency)))
    (is (null (getf m :wavelength)))
    (is (< (abs (getf m :speed)) 1d-6))))

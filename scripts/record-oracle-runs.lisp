;;;; scripts/record-oracle-runs.lisp — прогоны ручной модели этапа 3 в runs/.
;;;;
;;;; Пишет два прогона бегущей волны натяжения: в воде (2 Гц, λ = 1,5 L, 3 с)
;;;; и на агаре (0,5 Гц, λ = 0,65 L, 8 с). Их открывает просмотрщик
;;;; (viewer/worm-viewer.html) — по файлам или через ?run=<id> по HTTP.
;;;;
;;;; Запуск из корня репозитория (несколько минут):
;;;;   sbcl --non-interactive --no-userinit --load scripts/init.lisp --load scripts/record-oracle-runs.lisp
;;;;
;;;; Каталоги называются oracle-water и oracle-agar и перезаписываются.

(asdf:load-system "worm/oracle")

(defpackage #:worm.record-runs (:use #:cl))
(in-package #:worm.record-runs)

(defun record (id &key params amplitude frequency wavelength time h)
  ;; окно метрик — не короче двух периодов волны
  (let* ((body (worm.oracle:build-body :params params :amplitude amplitude :frequency frequency
                                       :wavelength wavelength :ramp (/ 1d0 frequency)))
         (start (get-internal-real-time))
         (dir (worm.oracle:record-body-run body :time time :h h :every 0.01d0 :id id
                                                  :metrics-window (max 1d0 (/ 2d0 frequency)))))
    (format t "~&~A: ~,1F с счёта, каталог ~A~%" id
            (/ (- (get-internal-real-time) start) internal-time-units-per-second) dir)
    (with-open-file (in (worm.io:run-file dir "metrics.csv"))
      (loop for line = (read-line in nil) while line do (format t "  ~A~%" line)))))

(record "oracle-water" :params "water-v1" :amplitude 20d0 :frequency 2d0 :wavelength 1.5d0
                       :time 3d0 :h 5d-4)
(record "oracle-agar" :params "agar-a-v1" :amplitude 20d0 :frequency 0.5d0 :wavelength 0.65d0
                      :time 8d0 :h 1d-3)

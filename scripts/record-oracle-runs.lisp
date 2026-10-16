;;;; scripts/record-oracle-runs.lisp — прогоны ручной модели в runs/.
;;;;
;;;; Четыре прогона, которые открывает просмотрщик (viewer/worm-viewer.html)
;;;; — по файлам или через ?run=<id> по HTTP:
;;;;
;;;;   oracle-water          кинематическое натяжение этапа 3, 2 Гц, λ = 1,5 L, 3 с;
;;;;   oracle-agar           то же на агаре, 0,5 Гц, λ = 0,65 L, 8 с;
;;;;   oracle-water-muscles  мышцы под генератором ритма, набор water-v1 как есть
;;;;                         (1,76 Гц, 1,54 L), 4 с;
;;;;   oracle-agar-muscles   мышцы, набор agar-a-v1 (0,30 Гц, 0,65 L), 12 с.
;;;;
;;;; Запуск из корня репозитория (несколько минут):
;;;;   sbcl --non-interactive --no-userinit --load scripts/init.lisp --load scripts/record-oracle-runs.lisp
;;;;
;;;; Каталоги перезаписываются.

(asdf:load-system "worm/oracle")

(defpackage #:worm.record-runs (:use #:cl))
(in-package #:worm.record-runs)

(defun record (id &key params muscles amplitude frequency wavelength time h)
  (let* ((set (worm.params:load-param-set params))
         (f (or frequency (worm.params:param-value set :gen-frequency)))
         (body (worm.oracle:build-body :params set :muscles muscles :amplitude amplitude
                                       :frequency frequency :wavelength wavelength
                                       :ramp (/ 1d0 f)))
         (start (get-internal-real-time))
         ;; окно метрик — не короче двух периодов волны
         (dir (worm.oracle:record-body-run body :time time :h h :every 0.01d0 :id id
                                                :metrics-window (max 1d0 (/ 2d0 f)))))
    (format t "~&~A: ~,1F с счёта, каталог ~A~%" id
            (/ (- (get-internal-real-time) start) internal-time-units-per-second) dir)
    (with-open-file (in (worm.io:run-file dir "metrics.csv"))
      (loop for line = (read-line in nil) while line do (format t "  ~A~%" line)))))

(record "oracle-water" :params "water-v1" :amplitude 15d0 :frequency 2d0 :wavelength 1.5d0
                       :time 3d0 :h 5d-4)
(record "oracle-agar" :params "agar-a-v1" :amplitude 15d0 :frequency 0.5d0 :wavelength 0.65d0
                      :time 8d0 :h 1d-3)
(record "oracle-water-muscles" :params "water-v1" :muscles t :time 4d0 :h 5d-4)
(record "oracle-agar-muscles" :params "agar-a-v1" :muscles t :time 12d0 :h 1d-3)

;;;; src/analysis/wcon.lisp — экспорт средней линии в WCON (этап 13).
;;;;
;;;; WCON — формат Worm tracker Commons (openworm/tracker-commons): объект
;;;; JSON с обязательными units и data. Запись data — одна особь: id, массив
;;;; времён t и для каждого времени массивы x и y точек средней линии.
;;;; Средняя линия — центры колец; плоскость WCON — x, y модели: в покое ось
;;;; x — от головы к хвосту, y — дорсальная сторона, поэтому изгиб тела и в
;;;; воде, и на агаре лежит в этой плоскости. Голова — первая точка
;;;; ("head": "L"); вентральная сторона (−y) — по часовой стрелке от
;;;; направления голова → хвост ("ventral": "CW"). Сведения о прогоне — в
;;;; metadata и в собственном поле "@Wormlisp".

(in-package #:worm.analysis)

(defun round-to (x digits)
  (let ((k (expt 10 digits))) (/ (fround (* x k)) k)))

(defun wcon-object (run &key (every 1) (digits 5))
  "Объект WCON (хэш-таблица) по кадрам прогона; EVERY — каждый который кадр."
  (let* ((frames (run-data-frames run))
         (n (worm.io:frames-file-count frames))
         (rings (run-data-rings run))
         (centers (make-dvec (* 3 rings)))
         (picked (loop for k from 0 below n by every collect k))
         (meta (run-data-meta run)))
    (flet ((spine (k c)
             (worm.topology:ring-centers (frame-dvec run k) rings (run-data-ring-nodes run) centers)
             (map 'vector (lambda (i) (round-to (aref centers (+ (* 3 i) c)) digits))
                  (loop for i below rings collect i))))
      (worm.io:json-object
       "units" (worm.io:json-object "t" "s" "x" "mm" "y" "mm")
       "metadata" (worm.io:json-object
                   "media" (or (meta-get meta "medium") "unknown")
                   "software" (worm.io:json-object
                               "tracker" (worm.io:json-object
                                          "name" "Wormlisp"
                                          "version" (or (meta-get meta "code" "commit") "unknown"))
                               "featureID" "@Wormlisp"))
       "@Wormlisp" (worm.io:json-object
                    "model" (or (meta-get meta "model" "name") 'null)
                    "params" (or (meta-get meta "params" "set") 'null)
                    "rings" rings
                    "note" "simulated worm: spine = ring centers, plane x-y of the model")
       "data" (vector
               (worm.io:json-object
                "id" "1"
                "t" (map 'vector (lambda (k) (round-to (aref (run-data-times run) k) 6)) picked)
                "x" (map 'vector (lambda (k) (spine k 0)) picked)
                "y" (map 'vector (lambda (k) (spine k 1)) picked)
                "head" "L"
                "ventral" "CW"))))))

(defun write-wcon (dir &key (every 1) path)
  "Записать среднюю линию прогона из каталога DIR в WCON; по умолчанию —
kinematics.wcon рядом с кадрами. Путь."
  (worm.io:write-json-file (or path (worm.io:run-file dir "kinematics.wcon"))
                           (wcon-object (load-run dir) :every every)))

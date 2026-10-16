;;;; tests/oracle/record.lisp — запись прогона ручной модели в каталог прогона.
;;;;
;;;; До этапа 8, где появится рантайм с собственной записью, прогоны ручной
;;;; модели пишутся отсюда: runs/<id>/ с config.sexp, meta.json, frames.bin,
;;;; metrics.csv, log.txt и status (архитектура, «Каталог прогона»).

(in-package #:worm.oracle)

(defun iso-utc ()
  (multiple-value-bind (s mi h d mo y) (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" y mo d h mi s)))

(defun body-actuation (body)
  (let ((p (body-pvec body)))
    (loop for name in '(:amplitude :frequency :wavelength :direction :ramp :phase :antagonist)
          collect name collect (aref p (position name +p-layout+)))))

(defun write-config (path body &key h time every)
  (let ((set (body-params body)))
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create
                              :external-format :utf-8)
      (with-standard-io-syntax
        (let ((*print-case* :downcase) (*print-readably* nil))
          (format out ";;;; Полная конфигурация прогона ручной модели (этап 3).~%")
          (pprint `(run :model oracle-body
                        :params ,(worm.params:param-set-name set)
                        :params-hash ,(worm.params:param-set-hash set)
                        :medium ,(body-medium body)
                        :kernels ,(body-kernels body)
                        :dt ,h :duration ,time :frames-every ,every
                        :actuation ,(body-actuation body)
                        :internal-values
                        ,(loop for name in (worm.params:param-names set)
                               for v = (worm.params:param-entry-value (worm.params:param-entry-of set name))
                               collect (list name (if (typep v 'dvec) (coerce v 'list) v)))
                        :units (:length :mm :time :s :force :un))
                  out)
          (terpri out))))))

(defun write-meta (path body &key h every count started finished restarts)
  (let* ((tube (body-tube body))
         (set (body-params body))
         (muscles (worm.anatomy:muscle-binding-muscles (body-binding body)))
         (act (body-actuation body)))
    (worm.io:write-json-file
     path
     (worm.io:json-object
      "format" 1
      "code" (worm.io:json-object "commit" (or (git-revision) (quote null)) "dirty" (quote null)
                                  "sbcl" (worm.platform:implementation-description) "codegen" 0)
      "model" (worm.io:json-object "name" "oracle-body" "hash" (quote null)
                                   "rings" (worm.topology:tube-rings tube)
                                   "ringNodes" (worm.topology:tube-ring-nodes tube))
      "params" (worm.io:json-object "set" (string-downcase (worm.params:param-set-name set))
                                    "hash" (worm.params:param-set-hash set))
      "units" (worm.io:json-object "length" "mm" "time" "s" "force" "uN")
      "medium" (string-downcase (body-medium body))
      "dt" (vector (worm.io:json-object "fromStep" 0 "value" h))
      "frames" (worm.io:json-object "every" every "count" count)
      "restarts" (coerce restarts 'vector)
      "started" started "finished" finished
      "topology" (worm.io:json-object "rings" (worm.topology:tube-rings tube)
                                      "ringNodes" (worm.topology:tube-ring-nodes tube)
                                      "length" (worm.topology:tube-length tube)
                                      "radiusMax" (worm.topology:tube-radius-max tube))
      "muscles" (map 'vector (lambda (m)
                               (worm.io:json-object
                                "name" (string-downcase (worm.anatomy:muscle-name m))
                                "quadrant" (string-downcase (worm.anatomy:muscle-quadrant m))
                                "row" (string-downcase (worm.anatomy:muscle-row m))
                                "from" (worm.anatomy:muscle-from m)
                                "peak" (worm.anatomy:muscle-peak m)
                                "to" (worm.anatomy:muscle-to m)))
                     muscles)
      "actuation" (worm.io:json-object "amplitude" (getf act :amplitude)
                                       "frequency" (getf act :frequency)
                                       "wavelength" (getf act :wavelength)
                                       "direction" (getf act :direction))))))

(defun record-body-run (body &key (time 1d0) (h 5d-4) (every 0.01d0) id (root (worm.io:runs-directory))
                                  (metrics-window 1d0))
  "Прогнать модель BODY TIME секунд с шагом H и записать кадр каждые EVERY
секунд. Возвращает каталог прогона."
  (let* ((dir (worm.io:make-run-directory :id (or id (worm.io:make-run-id)) :root root))
         (tube (body-tube body))
         (n (worm.topology:tube-node-count tube))
         (m 95)
         (it (make-body-integrator body :h h))
         (per-frame (max 1 (round every h)))
         (frames (round time (* per-frame h)))
         (writer (worm.io:open-frames-writer (worm.io:run-file dir "frames.bin") n m))
         (acts (make-fvec m))
         (amp (aref (body-pvec body) (position :amplitude +p-layout+)))
         (started (iso-utc)))
    (flet ((record ()
             (let ((syn (worm.solver:integrator-syn it)))
               (dotimes (k m)
                 (setf (aref acts k) (if (plusp amp)
                                         (float (max 0d0 (min 1d0 (/ (abs (aref syn k)) amp))) 0f0)
                                         0f0))))
             (worm.io:write-frame writer (worm.solver:integrator-step it) (worm.solver:integrator-time it)
                                  (worm.solver:integrator-y-mech it) acts)))
      (write-config (worm.io:run-file dir "config.sexp") body :h h :time time :every (* per-frame h))
      (unwind-protect
           (progn
             (record)
             (dotimes (k frames)
               (worm.solver:run-steps it per-frame)
               (record)))
        (worm.io:close-frames-writer writer))
      (write-meta (worm.io:run-file dir "meta.json") body :h h :every (* per-frame h)
                  :count (worm.io:frames-writer-count writer) :started started :finished (iso-utc)
                  :restarts (mapcar (lambda (r) (format nil "~(~{~A~^ ~}~)" r))
                                    (worm.solver:integrator-restarts it)))
      (let ((run (worm.analysis:load-run dir)))
        (worm.analysis:write-metrics-csv (worm.io:run-file dir "metrics.csv")
                                         (worm.analysis:metrics-windows run metrics-window)))
      (with-open-file (out (worm.io:run-file dir "log.txt") :direction :output :if-exists :supersede
                                                           :if-does-not-exist :create :external-format :utf-8)
        (format out "~A начало, ручная модель, набор ~(~A~), h = ~F с~%" started
                (worm.params:param-set-name (body-params body)) h)
        (format out "~A конец, ~D кадров, шаг ~D~%" (iso-utc) (worm.io:frames-writer-count writer)
                (worm.solver:integrator-step it)))
      (worm.io:write-status dir "done")
      dir)))

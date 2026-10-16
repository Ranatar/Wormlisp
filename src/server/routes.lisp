;;;; src/server/routes.lisp — акцептор и маршруты HTTP.
;;;;
;;;; Архитектура, «Сервер живого просмотра и протокол»:
;;;;   GET /                    просмотрщик viewer/worm-viewer.html (и lib/, selftest-data.js)
;;;;   GET /runs                список прогонов (JSON)
;;;;   GET /runs/<id>/<файл>    файлы каталога прогона
;;;;   GET /live                WebSocket живого потока (live.lisp)
;;;;   GET /selftest/<файл>     эталонные файлы tests/data/ для самопроверки
;;;;   GET /check, /health      проверочная страница этапа 0 и ответ «ok»
;;;;
;;;; Имена прогонов и файлов сверяются с белыми списками: сервер не отдаёт
;;;; ничего вне viewer/, runs/ и эталонов.

(in-package #:worm.server)

(defclass worm-acceptor (hunchensocket:websocket-acceptor hunchentoot:easy-acceptor)
  ()
  (:default-initargs :websocket-timeout 3600)
  (:documentation "Hunchentoot с WebSocket /live и маршрутами просмотрщика."))

(defvar *read-only* nil "Запрещены ли управляющие сообщения клиентов (serve :read-only t).")

(defparameter +run-files+
  '(("meta.json" . "application/json") ("frames.bin" . "application/octet-stream")
    ("metrics.csv" . "text/csv; charset=utf-8") ("config.sexp" . "text/plain; charset=utf-8")
    ("log.txt" . "text/plain; charset=utf-8") ("status" . "text/plain; charset=utf-8"))
  "Файлы каталога прогона, которые сервер отдаёт.")

(defparameter +selftest-files+
  '(("frames.bin" . "application/octet-stream") ("frames-expected.json" . "application/json")))

(defparameter +viewer-files+
  '(("lib/three.min.js" . "application/javascript") ("lib/OrbitControls.js" . "application/javascript")
    ("selftest-data.js" . "application/javascript")))

(defun safe-name-p (name)
  (and (plusp (length name)) (< (length name) 128)
       (every (lambda (c) (or (alphanumericp c) (find c "._-"))) name)
       (char/= (char name 0) #\.)))

(defun viewer-directory () (merge-pathnames "viewer/" (worm.core:project-root)))

(defun run-directory-of (id)
  "Каталог прогона ID: живого прогона процесса или runs/<id>/."
  (when (safe-name-p id)
    (let ((live (worm.runtime:find-run id)))
      (if (and live (worm.runtime:run-directory live))
          (worm.runtime:run-directory live)
          (let ((dir (merge-pathnames (format nil "~A/" id) (worm.io:runs-directory))))
            (and (probe-file dir) dir))))))

(defun send-file (path content-type)
  (if (and path (probe-file path))
      (hunchentoot:handle-static-file path content-type)
      (not-found)))

(defun not-found ()
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+
        (hunchentoot:content-type*) "text/plain; charset=utf-8")
  "нет такого файла")

(defun json-reply (object)
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8"
        (hunchentoot:header-out :cache-control) "no-store")
  (worm.io:json-string object))

;;; Список прогонов

(defun meta-of (dir)
  (let ((path (merge-pathnames "meta.json" dir)))
    (and (probe-file path) (ignore-errors (worm.io:read-json-file path)))))

(defun run-entry (id dir live)
  (let* ((meta (meta-of dir))
         (frames (and meta (gethash "frames" meta)))
         (count (if frames (gethash "count" frames 0) 0))
         (every (if frames (gethash "every" frames 0) 0)))
    (worm.io:json-object
     "id" id
     "model" (or (and meta (gethash "name" (gethash "model" meta (make-hash-table :test 'equal)))) 'null)
     "medium" (or (and meta (gethash "medium" meta)) 'null)
     "params" (or (and meta (gethash "set" (gethash "params" meta (make-hash-table :test 'equal)))) 'null)
     "frames" count
     "duration" (if (and (numberp count) (numberp every) (plusp count)) (* (1- count) every) 0)
     "status" (or (worm.io:read-status dir) 'null)
     "started" (or (and meta (gethash "started" meta)) 'null)
     "live" (and live t))))

(defun list-runs-json ()
  (let ((entries '()) (seen '()))
    (dolist (r worm.runtime:*runs*)
      (when (worm.runtime:run-directory r)
        (push (worm.runtime:run-id r) seen)
        (push (run-entry (worm.runtime:run-id r) (worm.runtime:run-directory r)
                         (not (member (worm.runtime:run-status r) '(:done :failed))))
              entries)))
    (dolist (dir (directory (merge-pathnames "*/" (worm.io:runs-directory))))
      (let ((id (car (last (pathname-directory dir)))))
        (when (and (safe-name-p id) (not (member id seen :test #'string=)) (meta-of dir))
          (push (run-entry id dir nil) entries))))
    (coerce (sort entries #'string> :key (lambda (h) (let ((s (gethash "started" h))) (if (stringp s) s ""))))
            'vector)))

;;; Диспетчер

(defun path-parts (path)
  (remove "" (uiop:split-string path :separator "/") :test #'string=))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor worm-acceptor) request)
  (let* ((path (hunchentoot:script-name request))
         (parts (path-parts path)))
    (cond
      ((string= path "/live") (call-next-method))
      ((null parts)
       (send-file (merge-pathnames "worm-viewer.html" (viewer-directory)) "text/html; charset=utf-8"))
      ((assoc (subseq path 1) +viewer-files+ :test #'string=)
       (send-file (merge-pathnames (subseq path 1) (viewer-directory))
                  (cdr (assoc (subseq path 1) +viewer-files+ :test #'string=))))
      ((and (equal parts '("runs")))
       (json-reply (list-runs-json)))
      ((and (= 3 (length parts)) (string= (first parts) "runs"))
       (let ((dir (run-directory-of (second parts)))
             (file (assoc (third parts) +run-files+ :test #'string=)))
         (if (and dir file)
             (progn (setf (hunchentoot:header-out :cache-control) "no-store")
                    (send-file (merge-pathnames (car file) dir) (cdr file)))
             (not-found))))
      ((and (= 2 (length parts)) (string= (first parts) "selftest"))
       (let ((file (assoc (second parts) +selftest-files+ :test #'string=)))
         (if file
             (send-file (merge-pathnames (format nil "tests/data/~A" (car file)) (worm.core:project-root)) (cdr file))
             (not-found))))
      (t (call-next-method)))))

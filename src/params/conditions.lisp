;;;; src/params/conditions.lisp — условия загрузки и правки параметров.
;;;;
;;;; Архитектура, «Ошибки»: params-error → missing-value, unit-mismatch,
;;;; out-of-bounds с перезапусками use-value и keep-old-value;
;;;; unknown-parameter — ветвь model-error. Синтаксические ошибки файла
;;;; набора (params-syntax-error) добавлены сюда же: их тоже исправляют
;;;; правкой файла, а не перезапуском.

(in-package #:worm.params)

(define-condition missing-value (params-error)
  ((names :initarg :names :initform nil :reader missing-value-names))
  (:report (lambda (c stream)
             (format stream "не заданы значения параметров (?): ~{~(~A~)~^, ~}"
                     (missing-value-names c)))))

(define-condition unit-mismatch (params-error) ())
(define-condition out-of-bounds (params-error) ())
(define-condition params-syntax-error (params-error) ())
(define-condition unknown-parameter (model-error) ())

(defun keep-old-value (&optional condition)
  "Вызвать перезапуск KEEP-OLD-VALUE, если он есть."
  (let ((restart (find-restart 'keep-old-value condition)))
    (when restart (invoke-restart restart))))

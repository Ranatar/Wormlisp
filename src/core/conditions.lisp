;;;; src/core/conditions.lisp — корень иерархии условий.
;;;;
;;;; Архитектура, «Ошибки»: все ошибки проекта — условия с корнем
;;;; worm-error. Здесь корень и ветви верхнего уровня; конкретные условия
;;;; определяют модули, которые их сигнализируют (anatomy-invalid —
;;;; worm.anatomy, missing-value — worm.params, numerical-blowup — решатель).

(in-package #:worm.core)

(define-condition worm-error (error)
  ((message :initarg :message :initform nil :reader worm-error-message
            :documentation "Готовый текст сообщения или NIL."))
  (:report (lambda (c stream)
             (format stream "~A" (or (worm-error-message c)
                                     (string-downcase (type-of c)))))))

(define-condition model-error (worm-error) ()
  (:documentation "Ошибка описания модели: defmodel, анатомия, параметры модели."))

(define-condition codegen-error (worm-error) ()
  (:documentation "Ошибка кодогенерации."))

(define-condition params-error (worm-error) ()
  (:documentation "Ошибка загрузки или правки параметров."))

(define-condition numerical-error (worm-error) ()
  (:documentation "Численный сбой шага решателя."))

(define-condition io-error (worm-error) ()
  (:documentation "Ошибка чтения или записи файлов."))

(define-condition protocol-error (worm-error) ()
  (:documentation "Искажённое сообщение клиента; в поток симуляции не попадает."))

(defun signal-worm-error (type control &rest args)
  "Сигнализировать условие TYPE без собственных слотов с сообщением,
собранным FORMAT из CONTROL и ARGS. Условия со слотами создаются через ERROR."
  (error type :message (apply #'format nil control args)))

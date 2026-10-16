;;;; src/dsl/conditions.lisp — ошибки модели с указанием компонента и формы.
;;;;
;;;; Архитектура, «Ошибки»: ошибка описания модели — условие model-error с
;;;; указанием компонента и формы, а не падение в середине кодогенерации.
;;;; Каждое условие языка модели несёт примесь dsl-location: имя компонента
;;;; (или экземпляра в defmodel) и форму, в которой найдена ошибка. Условия
;;;; других модулей — unknown-parameter (параметры), anatomy-invalid
;;;; (анатомия), ir-type-error и no-derivative (IR) — переподаются как их
;;;; подтипы с этой примесью, поэтому ловятся и по исходному типу.

(in-package #:worm.dsl)

;;; Имена языка сравниваются по имени символа, а не по пакету: компоненты
;;; пишутся в своих пакетах. Имена компонента хранятся ключевыми словами.

(defun kw (symbol) (intern (symbol-name symbol) :keyword))

(defun name-is (form name) (and (symbolp form) form (string= (symbol-name form) name)))

(define-condition dsl-location ()
  ((component :initarg :component :initform nil :reader dsl-location-component)
   (form :initarg :form :initform nil :reader dsl-location-form)))

(defun location-report (c stream)
  (format stream "~A~@[~%  компонент: ~(~A~)~]~@[~%  форма: ~(~S~)~]"
          (or (worm-error-message c) (string-downcase (type-of c)))
          (dsl-location-component c) (dsl-location-form c)))

(defmacro define-dsl-condition (name parents &optional slots doc)
  `(define-condition ,name (,@parents dsl-location) ,slots
     (:report location-report)
     ,@(when doc `((:documentation ,doc)))))

(define-dsl-condition dsl-syntax-error (model-error) ()
  "Форма компонента или модели не по грамматике языка.")
(define-dsl-condition unknown-name (model-error) ()
  "Имя в выражении или в описании модели ни к чему не относится.")
(define-dsl-condition unknown-component (model-error) ()
  "defmodel ссылается на неопределённый компонент.")
(define-dsl-condition unbound-input (model-error)
  ((input :initarg :input :initform nil :reader unbound-input-input))
  "Обязательный вход экземпляра не связан ни с одним выходом.")
(define-dsl-condition input-bound-twice (model-error) ()
  "Вход экземпляра связан больше одного раза.")
(define-dsl-condition algebraic-loop (model-error)
  ((cycle :initarg :cycle :initform nil :reader algebraic-loop-cycle))
  "Связи внутри подсистемы образуют алгебраическую петлю.")
(define-dsl-condition family-mismatch (model-error) ()
  "Семейство компонента, экземпляра или отношения :via не совпадает.")
(define-dsl-condition model-unknown-parameter (worm.params:unknown-parameter) ()
  "Параметра модели нет в реестре.")
(define-dsl-condition model-anatomy-invalid (worm.anatomy:anatomy-invalid) ()
  "Анатомия модели не загрузилась или не прошла проверку.")
(define-dsl-condition component-ir-type-error (worm.ir:ir-type-error) ()
  "Неверный тип или число аргументов в выражении компонента.")
(define-dsl-condition component-no-derivative (worm.ir:no-derivative) ()
  "В выражении компонента нужна производная примитива, которой нет.")

(defvar *component* nil "Компонент или экземпляр, который сейчас разбирается.")
(defvar *form* nil "Форма, которая сейчас разбирается.")

(defun dsl-error (type control &rest args)
  (error type :message (apply #'format nil control args) :component *component* :form *form*))

(defmacro with-location ((&key (component '*component*) (form '*form*)) &body body)
  "Тело с указанием места для ошибок; условия IR переподаются с местом."
  `(let ((*component* ,component) (*form* ,form))
     (handler-bind ((worm.ir:no-derivative
                      (lambda (c)
                        (unless (typep c 'dsl-location)
                          (error 'component-no-derivative :primitive (worm.ir:no-derivative-primitive c)
                                                          :message (worm-error-message c)
                                                          :component *component* :form *form*))))
                    (worm.ir:ir-type-error
                      (lambda (c)
                        (unless (typep c 'dsl-location)
                          (error 'component-ir-type-error :message (worm-error-message c)
                                                          :component *component* :form *form*)))))
       ,@body)))

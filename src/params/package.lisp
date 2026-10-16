;;;; src/params/package.lisp
;;;;
;;;; Слой 1. Параметры модели: единицы и их перевод во внутренние, реестр
;;;; параметров (имя, размерность, границы, живая правка), чтение наборов
;;;; из params/*.sexp с наследованием и отказ на значениях без единиц и
;;;; незаданных «?». Архитектура — «Представление модели» (параметры,
;;;; единицы) и «Хранилище и форматы файлов» (наборы параметров).

(defpackage #:worm.params
  (:use #:cl #:worm.core)
  (:export
   ;; единицы
   #:parse-unit #:unit-factor #:unit-dimension #:to-internal #:from-internal
   #:dimension-string #:+internal-units+
   ;; реестр
   #:define-parameter #:find-parameter #:list-parameters #:clear-registry
   #:parameter-spec #:parameter-spec-name #:parameter-spec-kind
   #:parameter-spec-unit #:parameter-spec-dimension #:parameter-spec-bounds
   #:parameter-spec-live-p #:parameter-spec-choices #:parameter-spec-doc
   ;; наборы
   #:param-set #:param-set-p #:param-set-name #:param-set-parent #:param-set-path
   #:param-set-entries #:param-set-hash #:param-set-canonical-text
   #:param-entry #:param-entry-name #:param-entry-value #:param-entry-unit
   #:param-entry-level #:param-entry-bounds #:param-entry-source #:param-entry-set
   #:load-param-set #:read-param-set #:*params-directory* #:derive-param-set
   #:param-value #:param-entry-of #:param-names #:missing-parameters
   #:require-params
   ;; условия
   #:missing-value #:missing-value-names
   #:unit-mismatch #:out-of-bounds #:unknown-parameter #:params-syntax-error
   #:keep-old-value))

;;;; src/codegen/emit.lisp — эмиссия типизированного Лиспа из ядер IR.
;;;;
;;;; Архитектура, «Кодогенерация», шаг 7: ядро становится циклом DOTIMES по
;;;; элементам с LET* из узлов IR. Узлы обходятся по номерам — порядок
;;;; создания, топологический и детерминированный, — поэтому одна модель
;;;; всегда даёт один и тот же текст. Переменные IR раскрываются в чтения
;;;; массивов (EMIT-LEAF), встроенные примитивы — в типизированные формы.
;;;;
;;;; Все массивы, которые нужны коду, — слоты элементов, поэлементные
;;;; параметры, карты рассеяния, буферы — передаются фабрике вектором ENV и
;;;; связываются с объявленными типами; в самом тексте нет ни чисел N, ни
;;;; смещений, зависящих от N.

(in-package #:worm.codegen)

;;; Окружение фабрики

(defstruct (env-plan (:copier nil))
  (items (make-array 0 :adjustable t :fill-pointer t))   ; значения
  (bindings '()))                                         ; ((символ тип номер) …), новые первыми

(deftype small-index () '(unsigned-byte 30))

(defparameter +env-types+
  '((:dvec . (simple-array double-float (*)))
    (:ivec . (simple-array fixnum (*)))
    (:fixnum . small-index)
    (:scratch . worm.topology:volume-scratch)))

(defun env-add (plan value type)
  "Добавить VALUE в окружение фабрики; символ, под которым код его читает."
  (let* ((k (fill-pointer (env-plan-items plan)))
         (sym (intern (format nil "E~D" k) :worm.codegen)))
    (vector-push-extend value (env-plan-items plan))
    (push (list sym (cdr (assoc type +env-types+)) k) (env-plan-bindings plan))
    sym))

(defun env-vector (plan) (coerce (env-plan-items plan) 'simple-vector))

(defun factory-form (plan lambda-list declarations body)
  "(lambda (env) (let ((E0 (svref env 0)) …) (lambda LAMBDA-LIST …)))."
  (let ((bindings (reverse (env-plan-bindings plan))))
    `(lambda (env)
       (declare (type simple-vector env) (ignorable env))
       (let ,(loop for (sym nil k) in bindings collect `(,sym (svref env ,k)))
         (declare ,@(loop for (sym type) in bindings collect `(type ,type ,sym))
                  (ignorable ,@(mapcar #'first bindings)))
         (lambda ,lambda-list
           (declare ,@declarations (optimize (speed 3) (safety 0) (debug 0)))
           ,@body)))))

;;; Узлы

(defun node-symbol (n) (intern (format nil "V~D" (worm.ir:node-id n)) :worm.codegen))

(defun reachable-nodes (roots)
  (let ((seen (make-hash-table)) (out '()))
    (labels ((walk (n)
               (unless (gethash (worm.ir:node-id n) seen)
                 (setf (gethash (worm.ir:node-id n) seen) t)
                 (loop for a across (worm.ir:node-args n) do (walk a))
                 (push n out))))
      (mapc #'walk roots))
    (sort out #'< :key #'worm.ir:node-id)))

(defun emit-nodes (roots leaf-form prim-form)
  "Связки LET* для узлов, достижимых из ROOTS, и формы самих корней.
LEAF-FORM: ключ переменной → форма чтения; PRIM-FORM: (имя индексы
аргументы) → форма или NIL (тогда общий вызов функции-значения)."
  (let ((forms (make-hash-table)) (bindings '()))
    (flet ((ref (n) (gethash (worm.ir:node-id n) forms)))
      (dolist (n (reachable-nodes roots))
        (let ((args (map 'list #'ref (worm.ir:node-args n))))
          (setf (gethash (worm.ir:node-id n) forms)
                (case (worm.ir:node-op n)
                  (:const (worm.ir:const-value n))
                  (:var (funcall leaf-form (worm.ir:var-key n)))
                  (t (let ((sym (node-symbol n))
                           (expr (case (worm.ir:node-op n)
                                   (:+ `(+ ,@args))
                                   (:* `(* ,@args))
                                   (:/ `(/ ,(first args) ,(second args)))
                                   (:expt `(* ,@(make-list (worm.ir:node-data n) :initial-element (first args))))
                                   (:sqrt `(sqrt (the (double-float 0d0) ,(first args))))
                                   (:log `(log (the (double-float (0d0)) ,(first args))))
                                   (:exp `(exp ,(first args)))
                                   (:tanh `(tanh ,(first args)))
                                   (:sin `(sin ,(first args)))
                                   (:cos `(cos ,(first args)))
                                   (:step `(if (> ,(first args) 0d0) 1d0 0d0))
                                   (:prim (destructuring-bind (name . indices) (worm.ir:node-data n)
                                            (or (funcall prim-form name indices args)
                                                `(funcall (the function (load-time-value
                                                                         (worm.ir:primitive-value (worm.ir:find-primitive ',name))))
                                                          ,@indices ,@args))))
                                   (t (error 'codegen-error :message (format nil "операция ~S без эмиссии" (worm.ir:node-op n)))))))
                       (push (list sym expr) bindings)
                       sym))))))
      (values (nreverse bindings) (mapcar #'ref roots)))))

(defun builtin-prim-form (volume)
  "Эмиссия встроенных примитивов. VOLUME — (scratch-символ), если в ядре
есть примитивы объёма."
  (lambda (name indices args)
    (let ((s (symbol-name name)))
      (cond ((string= s "POSITIVE") `(max 0d0 ,(first args)))
            ((string= s "CLAMP") `(min ,(third args) (max ,(second args) ,(first args))))
            ((string= s "HILL") `(%hill ,@args))
            ((string= s "HILL-DX") `(%hill-dx ,@args))
            ((and volume (eq name 'worm.dsl::volume))
             `(aref (worm.topology:volume-scratch-volume ,volume) 0))
            ((and volume (eq name 'worm.dsl::volume-grad))
             `(aref (worm.topology:volume-scratch-grad ,volume) ,(first indices)))
            ((and volume (eq name 'worm.dsl::volume-hess))
             `(aref (worm.topology:volume-scratch-hess ,volume) ,(first indices) ,(second indices)))
            (t nil)))))

(defun uses-primitive-p (roots &rest names)
  (some (lambda (n) (and (eq (worm.ir:node-op n) :prim) (member (car (worm.ir:node-data n)) names)))
        (reachable-nodes roots)))

(defun check-volume-arguments (roots ring-nodes)
  "Аргументы примитивов объёма — координаты слотов 0…2K−1 по порядку: тогда
их можно считать быстрой функцией по номерам узлов."
  (let ((expected (loop for j below (* 2 ring-nodes) append (loop for c below 3 collect (list :pos j c)))))
    (dolist (n (reachable-nodes roots))
      (when (and (eq (worm.ir:node-op n) :prim)
                 (member (car (worm.ir:node-data n)) '(worm.dsl::volume worm.dsl::volume-grad worm.dsl::volume-hess)))
        (unless (equal expected (map 'list (lambda (a) (and (worm.ir:var-p a) (worm.ir:var-key a))) (worm.ir:node-args n)))
          (error 'codegen-error :message "примитив объёма от выражений, а не от координат слотов отсека"))))))

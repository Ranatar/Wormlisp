;;;; src/codegen/tools.lisp — инструменты REPL для сгенерированного кода.
;;;;
;;;;   (show-kernel model 'pressure)            цикл ядра экземпляра, как он
;;;;                                            сгенерирован для сил
;;;;   (show-kernel model 'pressure :mech-matrix) — для матрицы шага
;;;;   (interpret-kernel model 'pressure 12 y)  значения корней ядра на одном
;;;;                                            элементе — эталонным интерпретатором
;;;; (архитектура, «Эталонный интерпретатор», «Детерминизм и читаемость»).

(in-package #:worm.codegen)

(defun find-unit (compiled instance)
  (let ((name (intern (symbol-name instance) :keyword)))
    (or (find name (model-layout-units (compiled-model-layout compiled)) :key #'unit-name)
        (error 'codegen-error :message (format nil "в модели нет экземпляра ~(~A~)" instance)))))

(defun generated-form (compiled instance &optional (function :mech-forces))
  "Сгенерированный цикл ядра INSTANCE для функции FUNCTION."
  (let* ((layout (compiled-model-layout compiled))
         (u (find-unit compiled instance))
         (*env* (make-env-plan))
         (*units* (model-layout-units layout))
         (*relations* (worm.dsl::model-instance-relations (model-layout-instance layout)))
         (names (coerce (model-layout-param-names layout) 'list))
         (kernel (kernel-of u)))
    (ecase function
      (:mech-forces
       (element-loop u (append (mapcar #'cdr (output-roots u (wired-ports u :mech)))
                               (mapcar #'third (worm.dsl::kernel-forces kernel)))
                     names (lambda (forms) (mapcar (lambda (f) `(write ,f)) forms))
                     :geometry (list *env*)))
      (:mech-matrix
       (cons 'progn (append (let ((jac (jacobian-entries u)))
                              (when jac
                                (list (element-loop u (mapcar #'fifth jac) names
                                                    (lambda (forms) (mapcar (lambda (f) `(scatter (* mh ,f))) forms))
                                                    :geometry (list *env*)))))
                            (loop for (nil k nil) in (structured-gradients u)
                                  collect (structured-gradient-loop u k names 0 0)))))
      ((:neural-rhs :neural-output)
       (element-loop u (append (mapcar #'cdr (worm.dsl::kernel-outputs kernel))
                               (mapcar #'cdr (worm.dsl::kernel-derivs kernel)))
                     names (lambda (forms) (mapcar (lambda (f) `(write ,f)) forms)))))))

(defun show-kernel (compiled instance &optional (function :mech-forces) (stream *standard-output*))
  "Напечатать сгенерированный код ядра INSTANCE."
  (let ((*print-right-margin* 110) (*print-case* :downcase) (*package* (find-package :worm.codegen)))
    (pprint (generated-form compiled instance function) stream)
    (terpri stream))
  (values))

(defun interpret-kernel (compiled instance element y &key aux (time 0d0))
  "Значения выходов, сил и производных ядра INSTANCE на элементе ELEMENT
при координатах Y — эталонным интерпретатором: ((корень . значение) …)."
  (let* ((u (find-unit compiled instance))
         (r (unit-running u))
         (kernel (kernel-of u))
         (values (worm.ir:run-evaluator (worm.dsl::running-evaluator r)
                                        (worm.dsl::element-env r element y aux time))))
    (append (loop for (name) in (worm.dsl::kernel-outputs kernel) collect (cons (list :output name) (pop values)))
            (loop for (slot c) in (worm.dsl::kernel-forces kernel) collect (cons (list :force slot c) (pop values)))
            (loop for (name) in (worm.dsl::kernel-derivs kernel) collect (cons (list :deriv name) (pop values))))))

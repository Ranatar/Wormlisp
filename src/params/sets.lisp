;;;; src/params/sets.lisp — наборы параметров params/*.sexp.
;;;;
;;;; Формат (архитектура, «Хранилище и форматы файлов»):
;;;;
;;;;   (params <имя> [:extends <родитель>] [:units (:length :mm :time :s :force :uN)]
;;;;     (<параметр> <значение> <единица> [:level L] [:bounds (min max)] [:source "..."])
;;;;     ...)
;;;;
;;;; Значение — число, список чисел, T/NIL, ключевое слово или «?» (не
;;;; задано). У числа единица обязательна, безразмерность пишется явно как
;;;; NIL: «значение без единиц» отвергается. У T/NIL, ключевых слов и целых
;;;; счётчиков единицы нет. Границы :bounds — в единицах самого значения.
;;;; Родитель ищется в том же каталоге как <родитель>.sexp, его значения
;;;; переопределяются потомком.
;;;;
;;;; Файл читается READ с *read-eval* = NIL в отдельном пакете, так что
;;;; чтение не исполняет код и не засоряет рабочие пакеты.

(in-package #:worm.params)

(defpackage #:worm.params.reader (:use #:cl))

(defvar *params-directory* nil
  "Каталог наборов параметров; NIL — params/ в корне репозитория.")

(defparameter +levels+ '(:measured :estimate :fit :numeric)
  "Степени достоверности: И, О, П, Ч документа параметров.")

(defparameter +entry-options+ '(:level :bounds :source))

(defstruct (param-entry (:copier nil))
  (name nil :type keyword :read-only t)
  (value nil :read-only t)  ; Во внутренних единицах; :missing — «?».
  (unit nil :read-only t)  ; Единица, как записана в файле.
  (level nil :read-only t)
  (bounds nil :read-only t)  ; (min max) из файла во внутренних единицах.
  (source nil :read-only t)
  (set nil :read-only t))  ; Имя набора, где значение задано.

(defstruct (param-set (:copier nil))
  (name nil :type keyword :read-only t)
  (parent nil :read-only t)
  (path nil :read-only t)
  (entries (make-hash-table :test 'eq) :read-only t))

(defmethod print-object ((s param-set) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~(~A~)~@[ ← ~(~A~)~], ~D параметров"
            (param-set-name s)
            (and (param-set-parent s) (param-set-name (param-set-parent s)))
            (hash-table-count (param-set-entries s)))))

;;; Чтение файла

(defun params-directory ()
  (or *params-directory* (merge-pathnames "params/" (project-root))))

(defun syntax-error (path control &rest args)
  (error 'params-syntax-error
         :message (format nil "~@[~A: ~]~?" (and path (file-namestring path)) control args)))

(defun read-param-set (path)
  "Прочитать форму набора из файла PATH без проверки по реестру."
  (with-open-file (in path :external-format :utf-8)
    (let ((form (with-standard-io-syntax
                  (let ((*read-eval* nil)
                        (*read-default-float-format* 'double-float)
                        (*package* (find-package '#:worm.params.reader)))
                    (handler-case (read in nil :eof)
                      (reader-error (e)
                        (syntax-error path "не читается: ~A" e)))))))
      (unless (and (consp form) (symbolp (first form))
                   (string= (symbol-name (first form)) "PARAMS"))
        (syntax-error path "ожидалась форма (params <имя> ...)"))
      form)))

(defun missing-marker-p (x)
  (and (symbolp x) (string= (symbol-name x) "?")))

(defun keyword-of (x) (and x (symbolp x) (normalize-name x)))

(defun split-header (path body)
  "Разделить тело формы набора на опции заголовка и записи."
  (let ((options '()))
    (loop while (and body (keywordp (first body)))
          do (unless (rest body)
               (syntax-error path "у опции ~S нет значения" (first body)))
             (push (first body) options)
             (push (second body) options)
             (setf body (cddr body)))
    (values (nreverse options) body)))

(defun check-units-header (path units)
  (loop for (dim unit) on units by #'cddr
        for expected = (getf +internal-units+ dim)
        do (unless (and expected (string-equal expected (string unit)))
             (syntax-error path "внутренние единицы модели — ~{~(~A~) ~A~^, ~}, а в заголовке ~(~A~) ~A"
                           +internal-units+ dim unit))))

(defun parse-entry (path form)
  "Запись набора → (values имя значение единица единица-задана-p опции)."
  (unless (and (consp form) (symbolp (first form)) (consp (rest form)))
    (syntax-error path "запись должна иметь вид (<параметр> <значение> <единица> ...): ~S" form))
  (destructuring-bind (name value &rest tail) form
    (let ((unit-given (and tail (not (member (first tail) +entry-options+)))))
      (let ((unit (and unit-given (first tail)))
            (options (if unit-given (rest tail) tail)))
        (unless (evenp (length options))
          (syntax-error path "у ~(~A~) нечётный список опций" name))
        (loop for (k nil) on options by #'cddr
              unless (member k +entry-options+)
                do (syntax-error path "у ~(~A~) неизвестная опция ~S" name k))
        (values (normalize-name name) value unit unit-given options)))))

;;; Проверка и перевод одной записи

(defun check-dimension (spec name unit)
  (let ((dims (unit-dimension unit)))
    (unless (equal dims (parameter-spec-dimension spec))
      (error 'unit-mismatch
             :message (format nil "~(~A~): единица ~A имеет размерность «~A», а параметр — «~A»"
                              name (or unit "nil") (dimension-string dims)
                              (dimension-string (parameter-spec-dimension spec)))))))

(defun convert-value (spec name value unit unit-given)
  "Проверить вид значения и перевести его во внутренние единицы."
  (when (missing-marker-p value)
    (when (and (member (parameter-spec-kind spec) '(:real :vector)) (not unit-given))
      (error 'unit-mismatch :message (format nil "~(~A~): значение без единиц" name)))
    (return-from convert-value :missing))
  (ecase (parameter-spec-kind spec)
    ((:real :vector)
     (unless unit-given
       (error 'unit-mismatch
              :message (format nil "~(~A~): значение ~S без единиц (безразмерность пишется явно: nil)"
                               name value)))
     (check-dimension spec name unit)
     (if (eq (parameter-spec-kind spec) :real)
         (if (realp value)
             (to-internal value unit)
             (error 'unit-mismatch :message (format nil "~(~A~): ожидалось число, а не ~S" name value)))
         (if (and (consp value) (every #'realp value))
             (dvec-from (mapcar (lambda (x) (to-internal x unit)) value))
             (error 'unit-mismatch :message (format nil "~(~A~): ожидался список чисел, а не ~S" name value)))))
    (:integer
     (when (and unit-given unit)
       (error 'unit-mismatch :message (format nil "~(~A~): счётчик не имеет единицы" name)))
     (if (integerp value) value
         (error 'unit-mismatch :message (format nil "~(~A~): ожидалось целое, а не ~S" name value))))
    (:boolean
     (when (and unit-given unit)
       (error 'unit-mismatch :message (format nil "~(~A~): у логического значения нет единицы" name)))
     (if (member value '(t nil)) value
         (error 'unit-mismatch :message (format nil "~(~A~): ожидалось t или nil, а не ~S" name value))))
    (:choice
     (when (and unit-given unit)
       (error 'unit-mismatch :message (format nil "~(~A~): у выбора нет единицы" name)))
     (let ((key (keyword-of value)))
       (if (member key (parameter-spec-choices spec)) key
           (error 'unit-mismatch
                  :message (format nil "~(~A~): ~S не из ~{~(~S~)~^ ~}" name value
                                   (parameter-spec-choices spec))))))))

(defun value-numbers (value)
  (etypecase value
    (real (list value))
    (dvec (coerce value 'list))
    (t nil)))

(defun check-bounds (name value bounds what)
  (when bounds
    (destructuring-bind (lo hi) bounds
      (dolist (x (value-numbers value))
        (unless (<= lo x hi)
          (restart-case
              (error 'out-of-bounds
                     :message (format nil "~(~A~) = ~F вне ~A [~F, ~F] (во внутренних единицах)"
                                      name x what lo hi))
            (continue ()
              :report "Принять значение, несмотря на границы."
              (return-from check-bounds value))))))
    value))

(defun make-entry (spec set-name name value unit unit-given options)
  (restart-case
      (let* ((internal (convert-value spec name value unit unit-given))
             (file-bounds (getf options :bounds))
             (bounds (and file-bounds
                          (if (and (consp file-bounds) (= 2 (length file-bounds))
                                   (every #'realp file-bounds))
                              (if (member (parameter-spec-kind spec) '(:real :vector))
                                  (list (to-internal (first file-bounds) unit)
                                        (to-internal (second file-bounds) unit))
                                  (list (float (first file-bounds) 0d0)
                                        (float (second file-bounds) 0d0)))
                              (error 'params-syntax-error
                                     :message (format nil "~(~A~): :bounds должны быть (min max)" name)))))
             (level (getf options :level)))
        (when (and level (not (member level +levels+)))
          (error 'params-syntax-error
                 :message (format nil "~(~A~): степень ~S не из ~{~(~S~)~^ ~}" name level +levels+)))
        (unless (eq internal :missing)
          (check-bounds name internal bounds "границ набора")
          (check-bounds name internal (parameter-spec-bounds spec) "жёстких границ реестра"))
        (make-param-entry :name name :value internal :unit unit :level level
                          :bounds bounds :source (getf options :source) :set set-name))
    (use-value (new)
      :report "Подставить значение во внутренних единицах."
      :interactive (lambda () (list (eval (read *query-io*))))
      (make-param-entry :name name :value new :unit unit :level (getf options :level)
                        :source "use-value" :set set-name))))

;;; Загрузка набора с наследованием

(defun set-path (name directory)
  (merge-pathnames (make-pathname :name (string-downcase (string name)) :type "sexp")
                   directory))

(defun load-param-set-file (path loading)
  (let* ((form (read-param-set path))
         (name (keyword-of (second form)))
         (directory (make-pathname :name nil :type nil :defaults path)))
    (unless name (syntax-error path "у набора нет имени"))
    (when (member name loading)
      (syntax-error path "наследование по кругу: ~{~(~A~)~^ ← ~}" (reverse (cons name loading))))
    (unless (string-equal (pathname-name path) (symbol-name name))
      (syntax-error path "набор ~(~A~) лежит в файле с другим именем" name))
    (multiple-value-bind (options entries) (split-header path (cddr form))
      (loop for (k nil) on options by #'cddr
            unless (member k '(:extends :units))
              do (syntax-error path "неизвестная опция набора ~S" k))
      (check-units-header path (getf options :units))
      (let* ((parent-name (keyword-of (getf options :extends)))
             (parent (and parent-name
                          (load-param-set-file (set-path parent-name directory)
                                               (cons name loading))))
             (set (make-param-set :name name :parent parent :path path))
             (table (param-set-entries set))
             (seen '()))
        (when parent
          (maphash (lambda (k v) (setf (gethash k table) v)) (param-set-entries parent)))
        (dolist (entry-form entries)
          (multiple-value-bind (pname value unit unit-given opts) (parse-entry path entry-form)
            (when (member pname seen)
              (syntax-error path "параметр ~(~A~) задан дважды" pname))
            (push pname seen)
            (setf (gethash pname table)
                  (make-entry (find-parameter pname) name pname value unit unit-given opts))))
        set))))

(defun load-param-set (designator &key (require :none) directory)
  "Загрузить набор параметров. DESIGNATOR — имя набора (символ или строка,
файл ищется в DIRECTORY или params/) или путь к файлу.
REQUIRE — :NONE, :ALL или список имён, которые обязаны быть заданы:
незаданные «?» среди них дают ошибку missing-value."
  (let* ((directory (or directory (params-directory)))
         (path (if (pathnamep designator) designator (set-path designator directory)))
         (set (load-param-set-file path '())))
    (unless (eq require :none)
      (require-params set require))
    (log-event :info 'params "загружен набор ~(~A~) из ~A, ~D параметров, хэш ~A"
               (param-set-name set) (file-namestring path)
               (hash-table-count (param-set-entries set)) (param-set-hash set))
    set))

;;; Доступ

(defun param-entry-of (set name)
  (gethash (normalize-name name) (param-set-entries set)))

(defun param-names (set)
  (sort (loop for k being the hash-keys of (param-set-entries set) collect k)
        #'string< :key #'symbol-name))

(defun missing-parameters (set &optional (names :all))
  "Имена параметров из NAMES (или всех), значение которых не задано или отсутствует."
  (let ((names (if (eq names :all) (param-names set) (mapcar #'normalize-name names))))
    (remove-if-not (lambda (n)
                     (let ((e (param-entry-of set n)))
                       (or (null e) (eq (param-entry-value e) :missing))))
                   names)))

(defun require-params (set names)
  "Ошибка missing-value со всем перечнем, если среди NAMES есть незаданные."
  (let ((missing (missing-parameters set names)))
    (when missing
      (error 'missing-value :names missing))
    set))

(defun param-value (set name)
  "Значение параметра во внутренних единицах; незаданное — ошибка missing-value."
  (let ((entry (param-entry-of set name)))
    (if (and entry (not (eq (param-entry-value entry) :missing)))
        (param-entry-value entry)
        (restart-case (error 'missing-value :names (list (normalize-name name)))
          (use-value (v)
            :report "Подставить значение во внутренних единицах."
            v)))))

;;; Канонический текст и хэш

(defun param-set-canonical-text (set)
  "Текст набора, не зависящий от порядка записей и единиц файла: имя и значение
во внутренних единицах для каждого параметра, по алфавиту."
  (with-standard-io-syntax
    (let ((*print-readably* nil) (*read-default-float-format* 'double-float))
      (with-output-to-string (out)
        (dolist (name (param-names set))
          (let ((v (param-entry-value (param-entry-of set name))))
            (format out "~(~A~) ~A~%" name
                    (typecase v
                      (dvec (format nil "(~{~S~^ ~})" (coerce v 'list)))
                      (t (prin1-to-string v))))))))))

(defun param-set-hash (set)
  (worm.platform:md5-hex (param-set-canonical-text set)))

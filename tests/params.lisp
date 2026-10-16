;;;; tests/params.lisp — единицы, реестр, наборы параметров.
;;;;
;;;; Условие готовности этапа 1: загрузчик отвергает значения без единиц и
;;;; незаданные «?».

(in-package #:worm.tests)

(def-suite params :in worm :description "worm.params.")
(in-suite params)

(defun close-to (a b &optional (rel 1d-12))
  (worm.core:approx= a b :rel rel))

(defmacro with-temp-params-dir ((dir) &body body)
  "Выполнить BODY с пустым временным каталогом наборов DIR."
  `(let ((,dir (uiop:ensure-directory-pathname
                (merge-pathnames (format nil "worm-params-~D-~D/" (get-universal-time) (random 1000000))
                                 (uiop:temporary-directory)))))
     (ensure-directories-exist ,dir)
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ,dir :validate t :if-does-not-exist :ignore))))

(defun write-set (dir name text)
  (with-open-file (out (merge-pathnames (format nil "~A.sexp" name) dir)
                       :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string text out)))

(defun load-text-set (text &key (require :none))
  "Записать набор TEXT с именем t1 во временный каталог и загрузить его."
  (with-temp-params-dir (dir)
    (write-set dir "t1" text)
    (worm.params:load-param-set "t1" :directory dir :require require)))

;;; Единицы

(test units-convert-to-internal
  (multiple-value-bind (f d) (worm.params:parse-unit :kpa)
    (is (= 1d3 f))
    (is (equal '(-2 0 1 0 0) d)))
  (multiple-value-bind (f d) (worm.params:parse-unit :un*s/mm^2)
    (is (= 1d0 f))
    (is (equal '(-2 1 1 0 0) d) "мкН·с/мм² — это Па·с"))
  (is (= 1d-3 (worm.params:unit-factor "mPa*s")))
  (is (= 1d6 (worm.params:unit-factor "MPa")))
  (is (= 1d-3 (worm.params:unit-factor :um)))
  (is (= 1d-3 (worm.params:unit-factor :ms)))
  (is (= 1d0 (worm.params:unit-factor :s)) "s в ключевом слове — секунда")
  (is (= 1d3 (worm.params:unit-factor "pA")) "пА = 1000 внутренних единиц тока")
  (signals worm.params:unit-mismatch (worm.params:parse-unit :pa))
  (is (equal '(0 1 0 0 0) (worm.params:unit-dimension "pF/pS")) "C/G — время")
  (is (= 1d0 (worm.params:unit-factor "pF/pS")))
  (is (equal '(0 0 0 0 0) (worm.params:unit-dimension nil))))

(test ambiguous-and-unknown-units-are-rejected
  (signals worm.params:unit-mismatch (worm.params:parse-unit :mpa))
  (signals worm.params:unit-mismatch (worm.params:parse-unit :furlong))
  (signals worm.params:unit-mismatch (worm.params:parse-unit "mm/s/s")))

;;; Реестр

(test registry-knows-model-parameters
  (let ((spec (worm.params:find-parameter 'bending-ei)))
    (is (equal '(2 0 1 0 0) (worm.params:parameter-spec-dimension spec)))
    (is (worm.params:parameter-spec-live-p spec)))
  (is (not (worm.params:parameter-spec-live-p (worm.params:find-parameter :body-length)))
      "длина тела требует пересборки")
  (signals worm.params:unknown-parameter (worm.params:find-parameter 'no-such-thing)))

;;; Наборы из params/

(test base-set-loads-in-internal-units
  (let ((s (worm.params:load-param-set "base-v1")))
    (is (= 1d0 (worm.params:param-value s 'body-length)))
    (is (close-to 0.04d0 (worm.params:param-value s 'radius-max)))
    (is (close-to 1.2d5 (worm.params:param-value s 'bulk-modulus)) "120 кПа = 1,2·10⁵ мкН/мм²")
    (is (close-to 0.1d0 (worm.params:param-value s 'tau-ca)))
    (is (close-to 0.22d0 (worm.params:param-value s 'bending-ei)))
    (is (close-to 2/3 (worm.params:param-value s 'head-weakening)))
    (is (equalp #(0.69d0 0.29d0) (worm.params:param-value s 'f-max-gradient)))
    (is (eq :posterior (worm.params:param-value s 'rf-direction)))
    (is (eql 12 (worm.params:param-value s 'units)))
    (is (close-to -35d0 (worm.params:param-value s 'neuron-e-leak)))
    (is (close-to 30d0 (worm.params:param-value s 'v-max)) "длин покоя в секунду")
    (is (null (worm.params:missing-parameters s)) "на этапе 5 заданы все параметры")))

(test media-sets-inherit-and-override
  (let ((water (worm.params:load-param-set "water-v1"))
        (agar (worm.params:load-param-set "agar-a-v1"))
        (agar-c (worm.params:load-param-set "agar-c-v1")))
    (is (= 3.3d-3 (worm.params:param-value water 'c-t)) "числа читаются в двойной точности")
    (is (null (worm.params:param-value water 'contact)))
    (is (eq t (worm.params:param-value agar 'contact)))
    (is (= 46d0 (worm.params:param-value agar-c 'c-t)) "потомок переопределяет")
    (is (= 400d0 (worm.params:param-value agar-c 'meniscus-force)) "и наследует остальное")
    (is (close-to 0.1d0 (worm.params:param-value agar-c 'tau-ca)) "через два поколения")
    (is (eq :agar-a-v1 (worm.params:param-entry-set (worm.params:param-entry-of agar-c 'contact))))
    (is (eq :measured (worm.params:param-entry-level (worm.params:param-entry-of agar-c 'c-t))))))

;;; Отказы

(test value-without-units-is-rejected
  (signals worm.params:unit-mismatch (load-text-set "(params t1 (tau-ca 40))"))
  (signals worm.params:unit-mismatch (load-text-set "(params t1 (tau-ca 40 :level :fit))"))
  (signals worm.params:unit-mismatch (load-text-set "(params t1 (f-max-gradient (0.7 0.3)))"))
  (signals worm.params:unit-mismatch (load-text-set "(params t1 (v-max ?))"))
  (finishes (load-text-set "(params t1 (tau-cuticle 5 :ms) (tail-softening 0.5 nil) (contact t))")))

(test unbound-question-mark-is-rejected
  (let ((text "(params t1 (tau-ca 40 :ms) (hill-n 3 nil) (v-max ? \"1/s\"))"))
    (signals worm.params:missing-value (load-text-set text :require :all))
    (handler-case (load-text-set text :require :all)
      (worm.params:missing-value (e)
        (is (equal '(:v-max) (worm.params:missing-value-names e)))))
    (finishes (load-text-set text :require '(tau-ca hill-n)))
    (let ((s (load-text-set text)))
      (is (equal '(:v-max) (worm.params:missing-parameters s)))
      (signals worm.params:missing-value (worm.params:param-value s 'v-max))
      (is (= 7d0 (handler-bind ((worm.params:missing-value
                                  (lambda (c) (declare (ignore c)) (use-value 7d0))))
                   (worm.params:param-value s 'v-max))))
      (signals worm.params:missing-value (worm.params:require-params s '(v-max hill-n))))))

(test wrong-dimension-is-rejected
  (signals worm.params:unit-mismatch (load-text-set "(params t1 (tau-ca 40 :mm))"))
  (signals worm.params:unit-mismatch (load-text-set "(params t1 (bending-ei 0.2 :un*mm))"))
  (signals worm.params:unit-mismatch (load-text-set "(params t1 (units 12 :mm))"))
  (signals worm.params:unit-mismatch (load-text-set "(params t1 (rf-direction :sideways))")))

(test unknown-parameter-is-rejected
  (signals worm.params:unknown-parameter (load-text-set "(params t1 (tau-cae 40 :ms))")))

(test bounds-are-checked-in-file-units
  (signals worm.params:out-of-bounds (load-text-set "(params t1 (tau-ca 40 :ms :bounds (50 500)))"))
  (finishes (load-text-set "(params t1 (tau-ca 40 :ms :bounds (5 500)))"))
  (signals worm.params:out-of-bounds (load-text-set "(params t1 (tail-softening 1.5 nil))")
    "жёсткие границы реестра")
  (let ((s (handler-bind ((worm.params:out-of-bounds #'continue))
             (load-text-set "(params t1 (tau-ca 40 :ms :bounds (50 500)))"))))
    (is (close-to 0.04d0 (worm.params:param-value s 'tau-ca)) "перезапуск continue принимает значение")))

(test malformed-sets-are-rejected
  (signals worm.params:params-syntax-error (load-text-set "(params t1 (tau-ca 40 :ms) (tau-ca 50 :ms))"))
  (signals worm.params:params-syntax-error (load-text-set "(params t1 (tau-ca 40 :ms :colour :red))"))
  (signals worm.params:params-syntax-error (load-text-set "(params t1 (tau-ca 40 :ms :level :guess))"))
  (signals worm.params:params-syntax-error (load-text-set "(params t1 :units (:length :m) (tau-ca 40 :ms))"))
  (signals worm.params:params-syntax-error (load-text-set "(parameters t1)"))
  (signals worm.params:params-syntax-error (load-text-set "(params t1 (tau-ca #.(+ 1 2) :ms))"))
  (with-temp-params-dir (dir)
    (write-set dir "t1" "(params t1 :extends t2 (tau-ca 40 :ms))")
    (write-set dir "t2" "(params t2 :extends t1 (hill-n 3 nil))")
    (signals worm.params:params-syntax-error (worm.params:load-param-set "t1" :directory dir))))

(test set-hash-follows-values-not-spelling
  (with-temp-params-dir (dir)
    (write-set dir "t1" "(params t1 (tau-ca 100 :ms) (radius-max 40 :um))")
    (write-set dir "t2" "(params t2 (radius-max 0.04 :mm) (tau-ca 0.1 :s))")
    (write-set dir "t3" "(params t3 (tau-ca 101 :ms) (radius-max 40 :um))")
    (let ((h1 (worm.params:param-set-hash (worm.params:load-param-set "t1" :directory dir)))
          (h2 (worm.params:param-set-hash (worm.params:load-param-set "t2" :directory dir)))
          (h3 (worm.params:param-set-hash (worm.params:load-param-set "t3" :directory dir))))
      (is (string= h1 h2) "те же значения в других единицах и другом порядке")
      (is (string/= h1 h3))
      (is (= 32 (length h1))))))

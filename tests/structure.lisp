;;;; tests/structure.lisp — раскладка систем по слоям.
;;;;
;;;; Таблица ниже — копия раздела «Модули и зависимости» архитектуры.
;;;; Если тест падает, расходятся либо worm.asd, либо документ; правится
;;;; то, что неверно, и только вместе с записью в журнал решений.

(in-package #:worm.tests)

(def-suite structure :in worm :description "Системы, пакеты и слои.")
(in-suite structure)

(defparameter *layers*
  '(("platform" 0 ()) ("core" 0 ("platform"))
    ("ir" 1 ("core")) ("topology" 1 ("core")) ("anatomy" 1 ("core")) ("params" 1 ("core"))
    ("dsl" 2 ("ir" "topology" "anatomy" "params"))
    ("components" 3 ("dsl"))
    ("codegen" 4 ("dsl" "ir")) ("solver" 4 ("core" "platform")) ("io" 4 ("core" "topology"))
    ("runtime" 5 ("codegen" "solver" "io" "components"))
    ("server" 6 ("runtime" "io")) ("batch" 6 ("runtime")) ("analysis" 6 ("io" "topology")))
  "(имя слой ожидаемые-зависимости-внутри-проекта)")

(defun project-deps (name)
  "Зависимости системы worm/NAME внутри проекта, без префикса worm/."
  (loop for dep in (asdf:system-depends-on (asdf:find-system (format nil "worm/~A" name)))
        when (and (stringp dep) (> (length dep) 5) (string= "worm/" dep :end2 5))
          collect (subseq dep 5)))

(defun layer-of (name) (second (assoc name *layers* :test #'string=)))

(test every-system-has-its-package
  (dolist (entry *layers*)
    (let ((name (first entry)))
      (is (asdf:find-system (format nil "worm/~A" name) nil) "нет системы worm/~A" name)
      (is (find-package (string-upcase (format nil "worm.~A" name))) "нет пакета worm.~A" name))))

(test dependencies-match-architecture
  (dolist (entry *layers*)
    (destructuring-bind (name layer expected) entry
      (declare (ignore layer))
      (is (null (set-exclusive-or (project-deps name) expected :test #'string=))
          "worm/~A: зависимости ~S, по архитектуре ~S" name (project-deps name) expected))))

(test dependencies-go-only-down
  (dolist (entry *layers*)
    (destructuring-bind (name layer expected) entry
      (declare (ignore expected))
      ;; Внутри слоя 0 worm/core опирается на worm/platform — это основание
      ;; всего проекта; во всех остальных случаях зависимость строго вниз.
      (dolist (dep (project-deps name))
        (is (or (< (layer-of dep) layer)
                (and (zerop layer) (string= dep "platform") (string/= name "platform")))
            "worm/~A (слой ~D) зависит от worm/~A (слой ~D)" name layer dep (layer-of dep))))))

(test hot-path-depends-only-on-layer-zero
  (dolist (dep (project-deps "solver"))
    (is (zerop (layer-of dep)) "решатель зависит от worm/~A" dep)))

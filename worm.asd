;;;; worm.asd — все системы проекта Wormlisp.
;;;;
;;;; Раскладка по слоям — архитектура, раздел «Модули и зависимости».
;;;; Зависимости идут только вниз; это проверяет тест layering в worm/tests.
;;;; Системы названы worm/<имя> (так ASDF разрешает держать их в одном
;;;; файле worm.asd), пакеты — worm.<имя>.

(defsystem "worm/platform"
  :description "Слой 0. Всё SBCL-специфичное."
  :depends-on ((:require "sb-md5"))
  :pathname "src/platform/"
  :serial t
  :components ((:file "package")
               (:file "platform")))

(defsystem "worm/core"
  :description "Слой 0. Типы, корневые условия, журнал, compiled-model."
  :depends-on ("worm/platform")
  :pathname "src/core/"
  :serial t
  :components ((:file "package")
               (:file "project")
               (:file "types")
               (:file "conditions")
               (:file "log")
               (:file "model")))

(defsystem "worm/ir"
  :description "Слой 1. Промежуточное представление выражений."
  :depends-on ("worm/core")
  :pathname "src/ir/"
  :components ((:file "package")))

(defsystem "worm/topology"
  :description "Слой 1. Трубка N x K: узлы, семейства элементов, геометрия."
  :depends-on ("worm/core")
  :pathname "src/topology/"
  :serial t
  :components ((:file "package")
               (:file "tube")
               (:file "geometry")))

(defsystem "worm/anatomy"
  :description "Слой 1. Мышцы, нейроны, синапсы, профиль радиуса."
  :depends-on ("worm/core")
  :pathname "src/anatomy/"
  :serial t
  :components ((:file "package")
               (:file "muscles")
               (:file "anatomy")))

(defsystem "worm/params"
  :description "Слой 1. Реестр параметров, единицы, наборы."
  :depends-on ("worm/core")
  :pathname "src/params/"
  :serial t
  :components ((:file "package")
               (:file "units")
               (:file "conditions")
               (:file "registry")
               (:file "sets")
               (:file "catalog")))

(defsystem "worm/dsl"
  :description "Слой 2. define-component, defmodel, связывание."
  :depends-on ("worm/ir" "worm/topology" "worm/anatomy" "worm/params")
  :pathname "src/dsl/"
  :components ((:file "package")))

(defsystem "worm/components"
  :description "Слой 3. Тело, мышцы, управление, среда на DSL."
  :depends-on ("worm/dsl")
  :pathname "src/components/"
  :components ((:file "package")))

(defsystem "worm/codegen"
  :description "Слой 4. Понижение модели, якобиан, эмиссия, компиляция."
  :depends-on ("worm/dsl" "worm/ir")
  :pathname "src/codegen/"
  :components ((:file "package")))

(defsystem "worm/solver"
  :description "Слой 4. Ленточная LU, линейно-неявный шаг, события."
  :depends-on ("worm/core" "worm/platform")
  :pathname "src/solver/"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "linalg")
               (:file "integrator")))

(defsystem "worm/io"
  :description "Слой 4. Кадры, файлы прогона, контрольные точки."
  :depends-on ("worm/core" "worm/topology" "com.inuoe.jzon")
  :pathname "src/io/"
  :serial t
  :components ((:file "package")
               (:file "frames")
               (:file "run")))

(defsystem "worm/runtime"
  :description "Слой 5. Поток симуляции, команды, снимки, API для REPL."
  :depends-on ("worm/codegen" "worm/solver" "worm/io" "worm/components")
  :pathname "src/runtime/"
  :components ((:file "package")))

(defsystem "worm/server"
  :description "Слой 6. HTTP и WebSocket, протокол живого просмотра."
  :depends-on ("worm/runtime" "worm/io" "hunchentoot")
  :pathname "src/server/"
  :components ((:file "package")
               (:file "check-page" :depends-on ("package"))))

(defsystem "worm/batch"
  :description "Слой 6. Развёртки, оркестратор, рабочие процессы."
  :depends-on ("worm/runtime")
  :pathname "src/batch/"
  :components ((:file "package")))

(defsystem "worm/analysis"
  :description "Слой 6. Метрики походки, кимограмма, сводки."
  :depends-on ("worm/io" "worm/topology")
  :pathname "src/analysis/"
  :serial t
  :components ((:file "package")
               (:file "gait")))

(defsystem "worm"
  :description "Симулятор локомоции C. elegans: фасадный пакет WORM для REPL."
  :author "Nyankonyan"
  :license "MIT"
  :version "0.0.1"
  :depends-on ("worm/server" "worm/batch" "worm/analysis")
  :pathname "src/"
  :components ((:file "package"))
  :in-order-to ((test-op (test-op "worm/tests"))))

(defsystem "worm/oracle"
  :description "Ручные ядра тела и среды (этап 3): эталон для кодогенерации."
  :depends-on ("worm/solver" "worm/topology" "worm/anatomy" "worm/params"
               "worm/io" "worm/analysis")
  :pathname "tests/oracle/"
  :serial t
  :components ((:file "package")
               (:file "kernels")
               (:file "body")
               (:file "record")))

(defsystem "worm/tests"
  :description "Тесты проекта (FiveAM)."
  :depends-on ("worm" "worm/oracle" "fiveam" "usocket" "cl-base64" "cl-ppcre")
  :pathname "tests/"
  :components ((:file "package")
               (:file "suite" :depends-on ("package"))
               (:file "structure" :depends-on ("suite"))
               (:file "platform" :depends-on ("suite"))
               (:file "core" :depends-on ("suite"))
               (:file "params" :depends-on ("suite"))
               (:file "topology" :depends-on ("suite"))
               (:file "anatomy" :depends-on ("suite"))
               (:file "solver-models" :depends-on ("suite"))
               (:file "solver" :depends-on ("solver-models"))
               (:file "body" :depends-on ("suite"))
               (:file "physics" :depends-on ("body"))
               (:file "io" :depends-on ("suite"))
               (:file "analysis" :depends-on ("io"))
               (:file "server" :depends-on ("suite")))
  :perform (test-op (o c) (uiop:symbol-call :worm.tests :run-all)))

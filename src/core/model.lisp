;;;; src/core/model.lisp — структура compiled-model.
;;;;
;;;; Архитектура, «Кодогенерация» (функции compiled-model) и «Решатель».
;;;; Это весь интерфейс между моделью и решателем: размеры векторов
;;;; состояния, ширина ленты механики и скомпилированные функции. Решатель
;;;; (этап 2) знает только эту структуру, поэтому его можно проверять на
;;;; моделях, написанных руками; кодогенерация (этап 7) заполняет её же.
;;;;
;;;; Соглашения о функциях (уточняются на этапах 2–3, где появятся первые
;;;; вызывающие стороны):
;;;;   mech-forces      (y p in g)        → G(y) в g: силы на узлы и dCa/dt
;;;;   mech-matrix      (y p in h band)   → лента B(y) − h·∂G/∂y
;;;;   neural-rhs       (y p in f)        → правая часть уровня 3
;;;;   neural-jacobian  (y p in jac)      → якобиан уровня 3
;;;;   neural-discrete  (y z p in out)    → шаг дискретного состояния уровня 2
;;;;   event-functions  (y z p out)       → значения функций событий
;;;;   observables      (y p out)         → кривизна, активации, центр масс
;;;; Отсутствующая функция — NIL (например, нервов уровня 3 нет на уровне 1).

(in-package #:worm.core)

(defstruct (compiled-model (:copier nil))
  (name nil :type symbol :read-only t)
  (hash nil :type (or null string) :read-only t)  ; Хэш канонического текста модели (sb-md5).
  (layout nil :read-only t)  ; Раскладка состояния: имена → индексы (state-layout, этап 6).
  (n-mech 0 :type index :read-only t)  ; Длина y-mech.
  (n-neural 0 :type index :read-only t)  ; Длина y-neural.
  (n-discrete 0 :type index :read-only t)  ; Длина z.
  (lower-band 0 :type index :read-only t)  ; Число поддиагоналей ленты механики.
  (upper-band 0 :type index :read-only t)  ; Число наддиагоналей ленты механики.
  (topology nil :read-only t)  ; Топология трубки (worm.topology).
  (anatomy nil :read-only t)  ; Анатомия (worm.anatomy).
  (mech-forces nil :type (or null function) :read-only t)
  (mech-matrix nil :type (or null function) :read-only t)
  (neural-rhs nil :type (or null function) :read-only t)
  (neural-jacobian nil :type (or null function) :read-only t)
  (neural-discrete nil :type (or null function) :read-only t)
  (event-functions nil :type (or null function) :read-only t)
  (observables nil :type (or null function) :read-only t))

(defmethod print-object ((m compiled-model) stream)
  (print-unreadable-object (m stream :type t)
    (format stream "~A y-mech ~D, лента ~D/~D"
            (compiled-model-name m) (compiled-model-n-mech m)
            (compiled-model-lower-band m) (compiled-model-upper-band m))))

;;;; src/core/model.lisp — структура compiled-model и раскладка ленты.
;;;;
;;;; Архитектура, «Кодогенерация» (функции compiled-model) и «Решатель».
;;;; Это весь интерфейс между моделью и решателем: размеры векторов и
;;;; скомпилированные функции. Решатель знает только эту структуру, поэтому
;;;; его можно проверять на моделях, написанных руками (этап 2), а
;;;; кодогенерация (этап 7) заполняет её же.
;;;;
;;;; Соглашения о функциях (этап 2). Все функции пишут результат в
;;;; переданные массивы и ничего не возвращают. Буферы результатов решатель
;;;; перед вызовом обнуляет, а функции только прибавляют к ним: так
;;;; сгенерированный код рассеивает вклады элементов без лишних проходов.
;;;;
;;;;   observables      (y-mech aux p obs)           наблюдаемые: кривизна колец, длины и
;;;;                                                 скорости мышц и т. п.
;;;;   neural-discrete  (y-neural obs z p clock)     шаг дискретного состояния z (уровень 2),
;;;;                                                 меняет z на месте
;;;;   neural-rhs       (y-neural obs z p clock f)   правая часть нервов уровня 3 при tₙ
;;;;   neural-jacobian  (y-neural obs z p clock jac) ∂f/∂y-neural, плотная матрица n×n
;;;;   neural-output    (y-neural obs z p clock syn) выход нервно-мышечной части при tₙ + h:
;;;;                                                 натяжение мышц (или их вход)
;;;;   mech-forces      (y-mech syn p g)             G(y): силы на узлы и правые части кальция
;;;;   mech-matrix      (y-mech aux syn p clock ab)  лента матрицы шага B(y) + h·∂(B(y)v)/∂y|v=aux − h·∂G/∂y
;;;;   check-state      (y-mech p)                   NIL или (values причина номер) —
;;;;                                                 проверки вроде V/V⁰ отсеков
;;;;   event-functions  (y-mech y-neural z p out)    значения функций событий (этап 10)
;;;;
;;;; aux — скорости y-mech прошлого шага, Δy/h (решение 22): решатель
;;;; записывает их после механического шага, если n-aux = n-mech, и хранит в
;;;; контрольных точках; при n-aux = 0 их нет. Через них модель читает
;;;; lagged-величины вроде скорости укорочения мышцы.
;;;;
;;;; clock — вектор из трёх чисел: tₙ (начало шага), h и номер шага. Время и
;;;; шаг передаются вектором, а не числами, и функции ничего не возвращают
;;;; (VALUES): иначе каждое число на границе вызова упаковывалось бы в кучу,
;;;; а шаг не должен выделять память.
;;;;
;;;; Отсутствующая функция — NIL: например, на уровне 1 нет нервов уровня 3,
;;;; а у механики без наблюдаемых нет observables.

(in-package #:worm.core)

(defstruct (compiled-model (:copier nil))
  (name nil :type symbol :read-only t)
  (hash nil :type (or null string) :read-only t)  ; хэш канонического текста модели (sb-md5)
  (layout nil :read-only t)            ; раскладка состояния: имена → индексы (этап 6)
  (n-mech 0 :type index :read-only t)        ; длина y-mech
  (n-neural 0 :type index :read-only t)      ; длина y-neural
  (n-discrete 0 :type index :read-only t)    ; длина z
  (n-observables 0 :type index :read-only t) ; длина obs
  (n-syn 0 :type index :read-only t)         ; длина syn (число мышц)
  (n-aux 0 :type index :read-only t)         ; длина aux: 0 или n-mech (скорости прошлого шага)
  (lower-band 0 :type index :read-only t)    ; число поддиагоналей ленты механики
  (upper-band 0 :type index :read-only t)    ; число наддиагоналей ленты механики
  (topology nil :read-only t)                ; топология трубки (worm.topology)
  (anatomy nil :read-only t)                 ; анатомия (worm.anatomy)
  (observables nil :type (or null function) :read-only t)
  (neural-discrete nil :type (or null function) :read-only t)
  (neural-rhs nil :type (or null function) :read-only t)
  (neural-jacobian nil :type (or null function) :read-only t)
  (neural-output nil :type (or null function) :read-only t)
  (mech-forces nil :type (or null function) :read-only t)
  (mech-matrix nil :type (or null function) :read-only t)
  (check-state nil :type (or null function) :read-only t)
  (event-functions nil :type (or null function) :read-only t))

(defmethod print-object ((m compiled-model) stream)
  (print-unreadable-object (m stream :type t)
    (format stream "~A y-mech ~D, лента ~D/~D~[~:;, y-neural ~:*~D~]"
            (compiled-model-name m) (compiled-model-n-mech m)
            (compiled-model-lower-band m) (compiled-model-upper-band m)
            (compiled-model-n-neural m))))

;;; Ленточная матрица.
;;;
;;; Хранение — как в LAPACK dgbtrf: массив из ldab = 2·kl + ku + 1 строк
;;; на n столбцов по столбцам; элемент A(i, j) (с нуля) лежит в строке
;;; kl + ku + i − j столбца j, то есть по индексу (kl + ku + i − j) + j·ldab.
;;; Верхние kl строк заняты заполнением при выборе ведущего элемента.
;;; Модель заполняет только исходную ленту: kl поддиагоналей и ku
;;; наддиагоналей.

(defconstant +clock-time+ 0 "Индекс tₙ в векторе clock.")
(defconstant +clock-h+ 1 "Индекс h в векторе clock.")
(defconstant +clock-step+ 2 "Индекс номера шага в векторе clock.")

(declaim (inline band-ldab band-index))

(defun band-ldab (kl ku)
  "Число строк хранения ленты с kl поддиагоналями и ku наддиагоналями."
  (+ (* 2 kl) ku 1))

(defun band-index (i j kl ku)
  "Индекс элемента A(i, j) в хранении ленты (i, j — с нуля)."
  (declare (type index i j kl ku))
  (+ (- (+ kl ku i) j) (* j (band-ldab kl ku))))

(defun make-band (n kl ku)
  "Пустое хранение ленты n × n."
  (make-dvec (* n (band-ldab kl ku))))

(defun band-ref (ab i j kl ku)
  "A(i, j) из хранения ленты; вне исходной ленты — 0."
  (if (<= (- j ku) i (+ j kl))
      (aref ab (band-index i j kl ku))
      0d0))

(defun (setf band-ref) (value ab i j kl ku)
  (unless (<= (- j ku) i (+ j kl))
    (error "элемент (~D, ~D) вне ленты ~D/~D" i j kl ku))
  (setf (aref ab (band-index i j kl ku)) value))

;;;; src/params/catalog.lisp — параметры модели в реестре.
;;;;
;;;; Список повторяет черновик набора в docs/parameters.md. Здесь задаются
;;;; размерность (через единицу), жёсткие физические границы и признак
;;;; живой правки; сами значения с источниками — в params/*.sexp.
;;;; На этапе 7 регистрация переедет в описания компонентов на DSL
;;;; (архитектура, «Порядок добавления компонента», шаг 2), а этот файл
;;;; останется только для параметров топологии и анатомии.

(in-package #:worm.params)

;;; Геометрия — входит в величины покоя, живой правки нет.
(define-parameter body-length    :unit :mm :bounds (0.1 10) :live nil
  :doc "Длина тела L.")
(define-parameter radius-max     :unit :um :bounds (1 200) :live nil
  :doc "Наибольший радиус тела R.")
(define-parameter radius-end-min :unit :um :bounds (0.1 50) :live nil
  :doc "Наименьший радиус крайних колец; задаёт запас эллипса профиля ε.")

;;; Кутикула и ткани
(define-parameter bending-ei     :unit :uN*mm^2 :bounds (0 100)
  :doc "Изгибная жёсткость тела EI в середине.")
(define-parameter tail-softening :unit nil :bounds (0 1)
  :doc "Доля, на которую хвостовая треть мягче середины.")
(define-parameter k-circ-ratio   :unit nil :bounds (0 1000) :doc "k_circ / k_c.")
(define-parameter k-shear-ratio  :unit nil :bounds (0 1000) :doc "k_shear / k_c.")
(define-parameter k-diam-ratio   :unit nil :bounds (0 1000) :doc "k_diam / k_c.")
(define-parameter tau-cuticle    :unit :ms :bounds (0 1000)
  :doc "Время релаксации рёбер τ_c = η_c / k_c.")

;;; Гидростатика
(define-parameter bulk-modulus   :unit :kPa :bounds (0 10000)
  :doc "Объёмный модуль тела, он же штраф объёма k_p.")
(define-parameter prestress      :unit :kPa :bounds (0 1000)
  :doc "Предварительное давление p₀.")

;;; Мышцы
(define-parameter tau-ca         :unit :ms :bounds (0.1 10000) :doc "Постоянная активации τ_Ca.")
(define-parameter ca-half        :unit nil :bounds (0 10) :doc "K_Ca, кальций нормирован на Ca_max.")
(define-parameter hill-n         :unit nil :bounds (0.5 10) :doc "Коэффициент Хилла n.")
(define-parameter f-max-mid      :unit :uN :bounds (0 1000) :doc "F_max клетки в середине тела.")
(define-parameter f-max-gradient :kind :vector :unit nil :bounds (0 1)
  :doc "Относительная F_max на переднем и заднем концах, линейно между ними.")
(define-parameter head-weakening :unit nil :bounds (0 1) :doc "Множитель первой клетки каждого ряда.")
(define-parameter max-shortening :unit nil :bounds (0 1) :doc "Предельное укорочение Δ_M.")
(define-parameter hill-a-ratio   :unit nil :bounds (0 10) :doc "a/F₀ гиперболы Хилла.")
(define-parameter v-max          :unit :mm/s :bounds (0 1000) :doc "Наибольшая скорость укорочения.")

;;; Нервное управление, уровень 2
(define-parameter units          :kind :integer :bounds (1 95) :live nil
  :doc "Число проприоцептивных единиц.")
(define-parameter rf-length      :unit nil :bounds (0 1) :doc "Длина рецептивного поля, доля L.")
(define-parameter rf-direction   :kind :choice :choices (:posterior :anterior)
  :doc "Направление рецептивного поля.")
(define-parameter hysteresis     :unit nil :bounds (0 1) :doc "Ширина гистерезиса ε B-нейрона.")
(define-parameter avb-ventral    :unit nil :bounds (0 10) :doc "Вход AVB вентральных B-нейронов.")
(define-parameter avb-dorsal     :unit nil :bounds (0 10) :doc "Вход AVB дорсальных B-нейронов.")

;;; Нервное управление, уровень 3
(define-parameter neuron-c       :unit :pF :bounds (0 1000) :doc "Ёмкость нейрона.")
(define-parameter neuron-g-leak  :unit :pS :bounds (0 100000) :doc "Проводимость утечки.")
(define-parameter neuron-e-leak  :unit :mV :bounds (-200 200) :doc "Потенциал утечки.")
(define-parameter contact-g      :unit :pS :bounds (0 100000) :doc "Проводимость одного контакта.")
(define-parameter e-exc          :unit :mV :bounds (-200 200) :doc "Потенциал реверсии возбуждающего синапса.")
(define-parameter e-inh          :unit :mV :bounds (-200 200) :doc "Потенциал реверсии тормозного синапса.")

;;; Среда
(define-parameter c-t            :unit :uN*s/mm^2 :bounds (0 1d6) :doc "Касательное сопротивление на единицу длины.")
(define-parameter c-n            :unit :uN*s/mm^2 :bounds (0 1d6) :doc "Нормальное сопротивление на единицу длины.")
(define-parameter contact        :kind :boolean :live nil :doc "Контакт с поверхностью (агар).")
(define-parameter meniscus-force :unit :uN/mm :bounds (0 1d5) :doc "Прижимающая сила мениска f_m.")
(define-parameter contact-stiffness :unit :uN/mm :bounds (0 1d9)
  :doc "Жёсткость штрафа проникновения в поверхность k_s (численная).")

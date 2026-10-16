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
(define-parameter ei-correction  :unit nil :bounds (0.1 10)
  :doc "Множитель k_c всех рёбер, при котором эффективная EI модели равна bending-ei.")
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
(define-parameter prestress      :unit :kPa :bounds (0 1000) :live nil
  :doc "Предварительное давление p₀. От него при сборке считаются натяжения покоя рёбер, поэтому живой правки нет.")

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
(define-parameter v-max          :unit "1/s" :bounds (0 1000)
  :doc "Наибольшая скорость укорочения, длин покоя в секунду.")
(define-parameter rate-filter    :unit :ms :bounds (0.01 1000)
  :doc "Постоянная сглаживания скорости укорочения, которую читает f_V.")
(define-parameter hill-eccentric :unit nil :bounds (1 3)
  :doc "Предел f_V при быстром удлинении под нагрузкой.")
(define-parameter syn-half       :unit nil :bounds (0 10)
  :doc "I½ сигмоиды Ca∞(I); вход нормирован, 1 — полный.")
(define-parameter syn-width      :unit nil :bounds (0.001 10) :doc "Ширина σ сигмоиды Ca∞(I).")

;;; Нервное управление, уровень 1 (генератор ритма)
(define-parameter gen-amplitude  :unit nil :bounds (0 10) :doc "Амплитуда A входа мышц, нормирована.")
(define-parameter gen-frequency  :unit :Hz :bounds (0 20) :doc "Частота бегущей волны входа.")
(define-parameter gen-wavelength :unit nil :bounds (0.05 20) :doc "Длина волны входа, доля L.")
(define-parameter gen-direction  :unit nil :bounds (-1 1) :doc "+1 — волна от головы к хвосту, −1 — обратно.")
(define-parameter gen-ramp       :unit :s :bounds (0.001 100) :doc "Время плавного нарастания входа.")
(define-parameter tension-amplitude :unit :uN :bounds (0 1000)
  :doc "Амплитуда кинематического натяжения мышцы (вариант мышцы этапа 3).")

;;; Нервное управление, уровень 2
(define-parameter units          :kind :integer :bounds (1 95) :live nil
  :doc "Число проприоцептивных единиц.")
(define-parameter rf-length      :unit nil :bounds (0 1) :live nil
  :doc "Длина рецептивного поля, доля L; веса поля считаются при сборке.")
(define-parameter head-sr-gain   :unit nil :bounds (0 100)
  :doc "Множитель проводимости рецептора головной единицы второго уровня.")
(define-parameter head-adapt-gain :unit nil :bounds (0 100)
  :doc "Утомление головной единицы второго уровня: ток против включённой стороны (0 — нет).")
(define-parameter head-bias      :unit nil :bounds (-10 10)
  :doc "Добавка ко входу VB головной единицы: запуск головного генератора.")
(define-parameter head-adapt-tau :unit :ms :bounds (1 100000)
  :doc "Постоянная утомления головной единицы.")
(define-parameter head-muscles-end :unit nil :bounds (0 1) :live nil
  :doc "При поле вперёд мышцы впереди этого положения (доля L) получают выход головной единицы.")
(define-parameter head-rf-length :unit nil :bounds (0 1) :live nil
  :doc "Поле назад головной единицы при поле вперёд у остальных (головной генератор), доля L.")
(define-parameter rf-direction   :kind :choice :choices (:posterior :anterior) :live nil
  :doc "Направление рецептивного поля: назад (Бойл) или вперёд (Wen).")
(define-parameter hysteresis     :unit nil :bounds (0 1) :doc "Ширина гистерезиса ε B-нейрона.")
(define-parameter b-threshold    :unit nil :bounds (0 10)
  :doc "Середина порогов B-нейрона: включение при θ(1 + ε), выключение при θ(1 − ε).")
(define-parameter avb-ventral    :unit nil :bounds (0 10) :doc "Вход AVB вентральных B-нейронов.")
(define-parameter avb-dorsal     :unit nil :bounds (0 10) :doc "Вход AVB дорсальных B-нейронов.")
(define-parameter sr-gain-base   :unit nil :bounds (0 10) :doc "Проводимость рецептора растяжения единицы n: (a + b·n)·s, a.")
(define-parameter sr-gain-slope  :unit nil :bounds (0 10) :doc "Рост проводимости рецептора к хвосту, b.")
(define-parameter sr-scale       :unit nil :bounds (0 100) :doc "Общий множитель s проводимости рецептора (у Бойла 1/4).")
(define-parameter dorsal-stretch-gain  :unit nil :bounds (0 10) :doc "Усиление дорсального рецептора при растяжении.")
(define-parameter dorsal-compress-gain :unit nil :bounds (0 10) :doc "Усиление дорсального рецептора при сжатии.")
(define-parameter vd-inhibition  :unit nil :bounds (0 10) :doc "Торможение VB от VD, возбуждаемого DB.")
(define-parameter sr-filter      :unit :ms :bounds (0.1 2000)
  :doc "Постоянная сглаживания сигнала рецептора растяжения τ_sr (задержка передачи изгиба).")

;; проводимостная версия B-нейрона (Boyle 2012, табл. 2)
(define-parameter gb-c           :unit :pF :bounds (0.01 1000) :doc "Ёмкость B-нейрона.")
(define-parameter gb-g-mem       :unit :pS :bounds (0 100000) :doc "Проводимость утечки B-нейрона.")
(define-parameter gb-i-act       :unit "pA" :bounds (0 1000)
  :doc "Наибольший самовозбуждающий ток I_act (у Бойла G_act = 20 при множителе σ без размерности).")
(define-parameter gb-i-gaba      :unit "pA" :bounds (0 1000) :doc "Наибольший ток торможения VB от VD.")
(define-parameter gb-g-avb       :unit :pS :bounds (0 100000) :doc "Проводимость щелевого контакта AVB.")
(define-parameter gb-v-rest      :unit :mV :bounds (-200 200) :doc "Потенциал покоя B-нейрона.")
(define-parameter gb-v-act       :unit :mV :bounds (-200 200) :doc "Середина самовозбуждающего тока.")
(define-parameter gb-v-avb       :unit :mV :bounds (-200 200) :doc "Потенциал AVB.")
(define-parameter gb-v-nmj       :unit :mV :bounds (-200 200) :doc "Середина нервно-мышечного синапса V₀,NMJ.")
(define-parameter gb-k-act       :unit nil :bounds (0 100) :doc "Крутизна самовозбуждающего тока, 1/мВ (у Бойла 500/В).")
(define-parameter gb-k-gaba      :unit nil :bounds (0 100) :doc "Крутизна торможения VD, 1/мВ.")
(define-parameter gb-k-nmj       :unit nil :bounds (0 100) :doc "Крутизна нервно-мышечного синапса, 1/мВ.")
(define-parameter gb-bias-ventral :unit "pA" :bounds (-1000 1000) :doc "Ток смещения вентральных B-нейронов.")
(define-parameter gb-w-ach       :unit nil :bounds (0 100) :doc "Вес возбуждающего нервно-мышечного синапса.")
(define-parameter gb-w-gaba      :unit nil :bounds (-100 0) :doc "Вес тормозного нервно-мышечного синапса.")
(define-parameter gb-sr-current  :unit "pA" :bounds (0 10000)
  :doc "Ток рецептора на единицу сигнала бинарной версии: I_sr = I·s·(a + b·n)·изгиб.")
(define-parameter gb-nmj-scale   :unit nil :bounds (0 100)
  :doc "Множитель входа мышц от проводимостной единицы (мышцы настроены на вход бинарной ±1).")

;;; Нервное управление, уровень 3
(define-parameter neuron-c       :unit :pF :bounds (0 1000) :doc "Ёмкость нейрона.")
(define-parameter neuron-g-leak  :unit :pS :bounds (0 100000) :doc "Проводимость утечки.")
(define-parameter neuron-e-leak  :unit :mV :bounds (-200 200) :doc "Потенциал утечки.")
(define-parameter contact-g      :unit :pS :bounds (0 100000) :doc "Проводимость одного контакта.")
(define-parameter e-exc          :unit :mV :bounds (-200 200) :doc "Потенциал реверсии возбуждающего синапса.")
(define-parameter e-inh          :unit :mV :bounds (-200 200) :doc "Потенциал реверсии тормозного синапса.")
(define-parameter syn-rise       :unit :Hz :bounds (0 1000) :doc "Скорость активации синапса a_r (Kunert 2014).")
(define-parameter syn-decay      :unit :Hz :bounds (0 1000) :doc "Скорость спада синапса a_d.")
(define-parameter syn-beta       :unit nil :bounds (0 10) :doc "Крутизна синаптической сигмоиды β, 1/мВ.")
(define-parameter command-current :unit "pA" :bounds (-1000 1000)
  :doc "Команда движения вперёд: ток +I в AVB и −I в AVA.")
(define-parameter neuron-rest-activity :unit nil :bounds (0.001 0.999) :live nil
  :doc "Сигмоида синапса φ_r в покое сети; по ней считаются пороги V_th (у Kunert 1/2).")
(define-parameter neuron-sr-conductance :unit :pS :bounds (0 1000000)
  :doc "Проводимость канала растяжения B-нейрона на единицу сигнала изгиба поля.")
(define-parameter gap-command-scale :unit nil :bounds (0 100) :live nil
  :doc "Масштаб щелевых контактов командных интернейронов с остальными нейронами.")
(define-parameter gap-motor-scale :unit nil :bounds (0 100) :live nil
  :doc "Масштаб остальных щелевых контактов.")
(define-parameter chemical-scale :unit nil :bounds (0 100) :live nil
  :doc "Масштаб химических синапсов между нейронами.")
(define-parameter head-drive     :unit :pS :bounds (0 1000000)
  :doc "Проверочный головной ритм третьего уровня: проводимость синусоидального канала нейронов вне тяжа со стороной.")
(define-parameter head-drive-frequency :unit :Hz :bounds (0 20) :doc "Частота проверочного головного ритма.")
(define-parameter nmj-head-scale :unit nil :bounds (0 100)
  :doc "Вес нервно-мышечных синапсов нейронов вне вентрального тяжа (головные, сублатеральные).")
(define-parameter nmj-inhibition-scale :unit nil :bounds (0 100)
  :doc "Вес тормозного (ГАМК) нервно-мышечного синапса относительно возбуждающего.")
(define-parameter neuron-nmj-gain :unit nil :bounds (0 1000)
  :doc "Вход мышцы на единицу активности синапса при среднем числе контактов на мышцу.")

;;; Среда
(define-parameter c-t            :unit :uN*s/mm^2 :bounds (0 1d6) :doc "Касательное сопротивление на единицу длины.")
(define-parameter c-n            :unit :uN*s/mm^2 :bounds (0 1d6) :doc "Нормальное сопротивление на единицу длины.")
(define-parameter contact        :kind :boolean :live nil :doc "Контакт с поверхностью (агар).")
(define-parameter meniscus-force :unit :uN/mm :bounds (0 1d5) :doc "Прижимающая сила мениска f_m.")
(define-parameter contact-stiffness :unit :uN/mm :bounds (0 1d9)
  :doc "Жёсткость штрафа проникновения в поверхность k_s (численная).")

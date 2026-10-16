;;;; params/base-v1.sexp — базовый набор параметров модели
;;;; Значения, единицы, степени достоверности и источники — по
;;;; docs/parameters.md; файл и документ правятся вместе. Степени:
;;;; :measured — И, :estimate — О, :fit — П, :numeric — Ч.

(params base-v1
  :units (:length :mm :time :s :force :uN)
  ;; геометрия
  (body-length     1      :mm            :level :measured  :source "Boyle 2012")
  (radius-max      40     :um            :level :measured  :source "Boyle 2012")
  (radius-end-min  5      :um            :level :numeric)
  ;; кутикула и ткани
  (bending-ei      0.22   :uN*mm^2       :level :measured  :bounds (0.095 0.28)
                                         :source "Backholm 2013, E_rod 110+-30 kPa; Fang-Yen 2010: 0.095")
  (tail-softening  0.5    nil            :level :measured  :source "Backholm 2013")
  (k-circ-ratio    1      nil            :level :fit       :bounds (0.1 10))
  (k-shear-ratio   0.3    nil            :level :fit       :bounds (0.1 1))
  (k-diam-ratio    0.03   nil            :level :fit       :bounds (0.01 0.1))
  (tau-cuticle     5      :ms            :level :estimate  :bounds (1 25)
                                         :source "Fang-Yen 2010: b_v/b < 5.3 ms; Boyle 2012: 10-25 ms")
  ;; гидростатика
  (bulk-modulus    120    :kPa           :level :measured  :bounds (80 160) :source "Gilpin 2015")
  (prestress       0      :kPa           :level :fit       :bounds (0 140))
  ;; мышцы
  (tau-ca          100    :ms            :level :estimate  :source "Boyle 2012")
  (ca-half         0.5    nil            :level :fit)
  (hill-n          3      nil            :level :fit       :bounds (2 4))
  (f-max-mid       10     :uN            :level :estimate  :bounds (2 40))
  (f-max-gradient  (0.69 0.29) nil       :level :estimate  :source "Boyle 2012, 0.70-0.42(m-1)/M")
  (head-weakening  2/3    nil            :level :estimate  :source "Boyle 2012, first muscle")
  (max-shortening  0.65   nil            :level :estimate  :source "Boyle 2012")
  (hill-a-ratio    0.25   nil            :level :fit)
  (v-max           ?      :mm/s          :level :fit)
  ;; уровень 2
  (units           12     nil            :level :estimate  :source "Boyle 2012")
  (rf-length       0.5    nil            :level :estimate  :bounds (0.2 0.5)
                                         :source "Boyle 2012: 0.5 L; Wen 2012: ~0.2 L")
  (rf-direction    :posterior nil        :level :fit
                                         :source "Boyle 2012: :posterior; Wen 2012: :anterior")
  (hysteresis      0.5    nil            :level :estimate  :source "Boyle 2012")
  (avb-ventral     1.175  nil            :level :estimate  :source "Boyle 2012, табл. 3")
  (avb-dorsal      0.675  nil            :level :estimate  :source "Boyle 2012, табл. 3")
  ;; уровень 3
  (neuron-c        1      :pF            :level :estimate  :source "Kunert 2014")
  (neuron-g-leak   10     :pS            :level :estimate  :source "Kunert 2014")
  (neuron-e-leak   -35    :mV            :level :estimate  :source "Kunert 2014")
  (contact-g       100    :pS            :level :estimate  :source "Kunert 2014")
  (e-exc           0      :mV            :level :estimate  :source "Kunert 2014")
  (e-inh           -45    :mV            :level :estimate  :source "Kunert 2014"))

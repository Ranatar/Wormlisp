;;;; src/dsl/families.lisp — семейства элементов, их геометрия и отношения.
;;;;
;;;; Семейство задаёт, какие узлы видит элемент (слоты) и какие
;;;; геометрические имена доступны его выражениям. Ядро строится одно на
;;;; пару «компонент × семейство»: выражения читают слоты относительно
;;;; элемента, а какие узлы стоят за слотами — знает топология.
;;;;
;;;;   рёбра (:long-edge :circ-edge :diagonal :diametral, общее :edge)
;;;;       слоты a, b; edge-vector, edge-length, length-rate, rest-length
;;;;   :compartment   слоты 0…2K−1 (переднее кольцо, затем заднее);
;;;;                  volume, rest-volume
;;;;   :ring          слоты 0…K−1; centroid, ds, rest-radius, position
;;;;   :node          слот self (0), узлы предыдущего кольца 1…K и следующего
;;;;                  K+1…2K (у концов — своё кольцо); ds (доля Δs кольца на
;;;;                  узел), position, tangent (касательная средней линии по
;;;;                  центрам соседних колец)
;;;;   :muscle        без слотов; position (максимум веретена), side (+1 —
;;;;                  дорсальная, −1 — вентральная)
;;;;   :global        один элемент без слотов
;;;;   :prop-unit     проприоцептивные единицы уровня 2 (этап 10): position, index
;;;;   :neuron        нейроны коннектома уровня 3 (этап 12): position (середина
;;;;                  иннервируемых мышц, у интернейронов — 0,5), side (+1 —
;;;;                  дорсальные мотонейроны DA, DB, DD, AS; −1 — вентральные VA,
;;;;                  VB, VD, VC; 0 — прочие), gaba (1 — ГАМК-ергический),
;;;;                  proprioceptive (1 — B-нейроны DB и VB), command (+1 — AVB,
;;;;                  −1 — AVA: команда вперёд возбуждает одного и тормозит другого),
;;;;                  cord (1 — мотонейроны вентрального тяжа и хвоста: AS, DA,
;;;;                  DB, DD, VA, VB, VC, VD, PDA, PDB), gap-total (сумма
;;;;                  щелевых контактов нейрона)
;;;;
;;;; Отношение :via связывает выход экземпляра одного семейства со входом
;;;; экземпляра другого и задаёт, как значения переходят между элементами.

(in-package #:worm.dsl)

(defparameter +edge-families+ '(:long-edge :circ-edge :diagonal :diametral))

(defparameter +families+
  (append '(:edge) +edge-families+ '(:compartment :ring :node :muscle :prop-unit :neuron :global)))

(defun edge-family-p (family) (or (eq family :edge) (member family +edge-families+)))

(defun family-accepts-p (component-family instance-family)
  "Можно ли поставить компонент семейства COMPONENT-FAMILY на INSTANCE-FAMILY."
  (or (eq component-family instance-family)
      (and (eq component-family :edge) (member instance-family +edge-families+))))

(defun family-geometry (family)
  "Геометрические имена, доступные выражениям семейства."
  (cond ((eq family :long-edge) '(edge-vector edge-length length-rate rest-length side radius-factor))
        ((edge-family-p family) '(edge-vector edge-length length-rate rest-length))
        ((eq family :compartment) '(volume rest-volume))
        ((eq family :ring) '(centroid ds rest-radius position))
        ((eq family :node) '(ds position tangent))
        ((eq family :muscle) '(position side))
        ((eq family :prop-unit) '(position index))
        ((eq family :neuron) '(position side gaba proprioceptive command cord gap-total))
        (t '())))

(defun family-slots (family ring-nodes)
  "Слоты элемента семейства."
  (cond ((edge-family-p family) '(:a :b))
        ((eq family :compartment) (loop for j below (* 2 ring-nodes) collect j))
        ((eq family :ring) (loop for j below ring-nodes collect j))
        ((eq family :node) (loop for j from 0 to (* 2 ring-nodes) collect j))
        (t '())))

(defun slot-designator (family form)
  "Слот из формы (pos <слот>) или NIL."
  (let ((name (and (consp form) (name-is (first form) "POS") (= (length form) 2) (second form))))
    (cond ((null name) nil)
          ((and (edge-family-p family) (symbolp name) (member (symbol-name name) '("A" "B") :test #'string=))
           (intern (symbol-name name) :keyword))
          ((and (eq family :node) (symbolp name) (string= (symbol-name name) "SELF")) 0)
          ((and (member family '(:compartment :ring :node)) (integerp name)) name)
          (t nil))))

;;; Отношения :via
;;;
;;;   :attachment       мышца ↔ продольное ребро, с весами привязки; в обе
;;;                     стороны — взвешенная сумма
;;;   :identity         то же семейство, тот же элемент
;;;   :broadcast        :global → любое семейство: одно значение всем
;;;   :innervation      единица управления → мышцы её участка тела, вес —
;;;                     сторона мышцы (+1 дорсальная, −1 вентральная); нейрон →
;;;                     мышца — по нервно-мышечным синапсам коннектома, вес —
;;;                     число контактов, делённое на сумму возбуждающих
;;;                     контактов мотонейронов тяжа на эту мышцу
;;;   :chemical         нейрон → нейрон по химическим синапсам, вес — число
;;;                     контактов (пре → пост)
;;;   :gap              нейрон ↔ нейрон по щелевым контактам, вес — число
;;;                     контактов, в обе стороны
;;;   :receptive-field  продольные рёбра рецептивного поля → единица, вес
;;;                     ±N_SR / (рёбер одной стороны в поле): сумма по полю,
;;;                     нормированная на его длину; минус — у поля вперёд
;;;                     (параметры, «Уровень 2»); для нейронов — то же поле назад
;;;                     от положения B-нейрона, у остальных нейронов поля нет

(defstruct (relation (:copier nil))
  name pairs documentation)

(defparameter +relations+
  (list (make-relation :name :attachment :pairs '((:muscle . :long-edge) (:long-edge . :muscle))
                       :documentation "мышца ↔ продольное ребро, взвешенная сумма")
        (make-relation :name :identity :pairs :same
                       :documentation "тот же элемент того же семейства")
        (make-relation :name :broadcast :pairs :from-global
                       :documentation "одно значение глобального элемента всем элементам")
        (make-relation :name :innervation :pairs '((:prop-unit . :muscle) (:neuron . :muscle))
                       :documentation "управление → мышца (этапы 10 и 12)")
        (make-relation :name :receptive-field :pairs '((:long-edge . :prop-unit) (:long-edge . :neuron))
                       :documentation "продольные рёбра рецептивного поля → проприоцептивная единица или B-нейрон")
        (make-relation :name :chemical :pairs '((:neuron . :neuron))
                       :documentation "химические синапсы коннектома, пре → пост (этап 12)")
        (make-relation :name :gap :pairs '((:neuron . :neuron))
                       :documentation "щелевые контакты коннектома, в обе стороны (этап 12)")))

(defun find-relation (name)
  (find name +relations+ :key #'relation-name))

(defun relation-allows-p (relation from to)
  (let ((pairs (relation-pairs relation)))
    (case pairs
      (:same (eq from to))
      (:from-global (eq from :global))
      (t (member (cons from to) pairs :test #'equal)))))

;;; Поэлементные массивы
;;;
;;; Поэлементный параметр компонента ((k-c :per-element)) после
;;; переименования называет массив, который строится из скалярных
;;; параметров и топологии при сборке модели (например, жёсткость рёбер из
;;; EI и профиля радиуса). Массивы регистрируются заранее, чтобы раскрытие
;;; модели проверяло имена.

(defstruct (element-array (:copier nil))
  name family compute documentation)

(defvar *element-arrays* (make-hash-table :test 'eq))

(defmacro define-element-array (name &key family compute documentation)
  "COMPUTE — функция (context) → dvec, где CONTEXT — plist
(:topology :params :anatomy :family :count)."
  `(setf (gethash (kw ',name) *element-arrays*)
         (make-element-array :name (kw ',name) :family ,family :compute ,compute
                             :documentation ,documentation)))

(defun find-element-array (name &optional (errorp t))
  (or (gethash (kw name) *element-arrays*)
      (and errorp (dsl-error 'model-unknown-parameter "нет поэлементного массива ~(~A~)" name))))

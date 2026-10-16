;;;; src/params/units.lisp — единицы измерения.
;;;;
;;;; Внутренняя система модели (архитектура, «Единицы»): миллиметры,
;;;; секунды, микроньютоны. Для нервной части добавлены милливольты и
;;;; фемтокулоны: тогда пФ, пСм и мВ — внутренние единицы, а C/G = пФ/пСм
;;;; даёт секунды. Отсюда производные: 1 Па = 1 мкН/мм², 1 Па·с = 1 мкН·с/мм²,
;;;; 1 пА = 1000 внутренних единиц тока (фА).
;;;;
;;;; Размерность — вектор показателей по базе (длина время сила напряжение
;;;; заряд). Единица записывается ключевым словом (:uN*s/mm^2) или строкой
;;;; ("mPa*s"). В ключевом слове регистр теряется при чтении, поэтому имена,
;;;; различающиеся только регистром (MPa и mPa), допускаются лишь строкой;
;;;; «s» в ключевом слове — секунда, сименс пишется строкой "S".

(in-package #:worm.params)

(defparameter +internal-units+ '(:length "mm" :time "s" :force "uN" :voltage "mV" :charge "fC")
  "Внутренние единицы модели по базовым размерностям.")

(defparameter +base-names+ '("длина" "время" "сила" "напряжение" "заряд"))

;;; Атомарные единицы: имя → (множитель во внутренние . размерность).
(defparameter +atomic-units+
  (let ((table (make-hash-table :test 'equal)))
    (flet ((def (name factor l tm f v q)
             (setf (gethash name table) (cons (float factor 0d0) (list l tm f v q)))))
      ;; длина
      (def "m" 1d3 1 0 0 0 0) (def "cm" 1d1 1 0 0 0 0) (def "mm" 1 1 0 0 0 0)
      (def "um" 1d-3 1 0 0 0 0) (def "nm" 1d-6 1 0 0 0 0)
      ;; время
      (def "s" 1 0 1 0 0 0) (def "ms" 1d-3 0 1 0 0 0) (def "us" 1d-6 0 1 0 0 0)
      (def "min" 60 0 1 0 0 0) (def "h" 3600 0 1 0 0 0)
      (def "Hz" 1 0 -1 0 0 0)
      ;; сила
      (def "N" 1d6 0 0 1 0 0) (def "mN" 1d3 0 0 1 0 0) (def "uN" 1 0 0 1 0 0)
      (def "nN" 1d-3 0 0 1 0 0) (def "pN" 1d-6 0 0 1 0 0)
      ;; давление: 1 Па = 1 Н/м² = 1 мкН/мм²
      (def "Pa" 1 -2 0 1 0 0) (def "kPa" 1d3 -2 0 1 0 0) (def "MPa" 1d6 -2 0 1 0 0)
      (def "mPa" 1d-3 -2 0 1 0 0)
      ;; напряжение, заряд
      (def "V" 1d3 0 0 0 1 0) (def "mV" 1 0 0 0 1 0)
      (def "C" 1d15 0 0 0 0 1) (def "pC" 1d3 0 0 0 0 1) (def "fC" 1 0 0 0 0 1)
      ;; ёмкость = заряд / напряжение
      (def "F" 1d12 0 0 0 -1 1) (def "nF" 1d3 0 0 0 -1 1) (def "pF" 1 0 0 0 -1 1)
      ;; проводимость = заряд / (время · напряжение)
      (def "S" 1d12 0 -1 0 -1 1) (def "nS" 1d3 0 -1 0 -1 1) (def "pS" 1 0 -1 0 -1 1)
      ;; ток = заряд / время
      (def "A" 1d15 0 -1 0 0 1) (def "nA" 1d6 0 -1 0 0 1) (def "pA" 1d3 0 -1 0 0 1)
      (def "fA" 1 0 -1 0 0 1))
    table))

;;; Как читаются имена из ключевых слов, где регистр потерян.
(defparameter +keyword-preference+ '(("S" . "s"))
  "Неоднозначности регистра, разрешённые для ключевых слов: S — секунда.")

(defun lookup-atomic (name case-sensitive)
  (if case-sensitive
      (or (gethash name +atomic-units+)
          (error 'unit-mismatch :message (format nil "неизвестная единица ~S" name)))
      (let ((pref (cdr (assoc name +keyword-preference+ :test #'string-equal))))
        (if pref
            (gethash pref +atomic-units+)
            (let ((matches (loop for key being the hash-keys of +atomic-units+
                                 when (string-equal key name) collect key)))
              (case (length matches)
                (0 (error 'unit-mismatch :message (format nil "неизвестная единица ~A" name)))
                (1 (gethash (first matches) +atomic-units+))
                (t (error 'unit-mismatch
                          :message (format nil "единица ~A неоднозначна (~{~A~^, ~}); запишите её строкой с нужным регистром"
                                           name (sort matches #'string<))))))))))

(defun split-string (string char)
  (loop with start = 0
        for pos = (position char string :start start)
        collect (subseq string start pos)
        while pos do (setf start (1+ pos))))

(defun parse-factor (token case-sensitive)
  "Сомножитель вида name или name^n → (множитель . размерность)."
  (let* ((caret (position #\^ token))
         (name (if caret (subseq token 0 caret) token))
         (power (if caret
                    (or (ignore-errors (parse-integer token :start (1+ caret)))
                        (error 'unit-mismatch :message (format nil "плохая степень в ~A" token)))
                    1)))
    (when (string= name "")
      (error 'unit-mismatch :message (format nil "пустая единица в ~A" token)))
    (if (string= name "1")
        (cons 1d0 (list 0 0 0 0 0))
        (destructuring-bind (factor . dims) (lookup-atomic name case-sensitive)
          (cons (expt factor power) (mapcar (lambda (d) (* d power)) dims))))))

(defun parse-unit (unit)
  "Разобрать единицу UNIT. Возвращает множитель во внутренние единицы и размерность.
UNIT — NIL (безразмерная), ключевое слово или строка."
  (cond
    ((null unit) (values 1d0 (list 0 0 0 0 0)))
    ((or (symbolp unit) (stringp unit))
     (let* ((case-sensitive (stringp unit))
            (text (if (stringp unit) unit (symbol-name unit)))
            (parts (split-string text #\/)))
       (when (> (length parts) 2)
         (error 'unit-mismatch :message (format nil "в единице ~A больше одной черты /" text)))
       (let ((factor 1d0) (dims (list 0 0 0 0 0)))
         (loop for part in parts
               for sign in '(1 -1)
               do (dolist (token (split-string part #\*))
                    (destructuring-bind (f . d) (parse-factor token case-sensitive)
                      (setf factor (if (plusp sign) (* factor f) (/ factor f))
                            dims (mapcar (lambda (a b) (+ a (* sign b))) dims d)))))
         (values factor dims))))
    (t (error 'unit-mismatch :message (format nil "единица должна быть символом или строкой, а не ~S" unit)))))

(defun unit-factor (unit) (nth-value 0 (parse-unit unit)))
(defun unit-dimension (unit) (nth-value 1 (parse-unit unit)))

(defun to-internal (value unit)
  "Перевести число VALUE из единиц UNIT во внутренние."
  (* (float value 0d0) (unit-factor unit)))

(defun from-internal (value unit)
  "Перевести число VALUE из внутренних единиц в UNIT."
  (/ (float value 0d0) (unit-factor unit)))

(defun dimension-string (dims)
  "Размерность для сообщений: «длина^-2 сила» или «безразмерная»."
  (let ((parts (loop for d in dims for name in +base-names+
                     unless (zerop d)
                       collect (if (= d 1) name (format nil "~A^~D" name d)))))
    (if parts (format nil "~{~A~^ ~}" parts) "безразмерная")))

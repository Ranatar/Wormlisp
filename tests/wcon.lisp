;;;; tests/wcon.lisp — экспорт WCON и проверка по схеме формата (этап 13).
;;;;
;;;; Схема — tests/data/wcon/wcon_schema.json из openworm/tracker-commons.
;;;; Проверка — подмножество JSON Schema, которым схема пользуется: type,
;;;; required, properties, patternProperties, additionalProperties, items,
;;;; minItems, minLength, enum, oneOf, anyOf и $ref на #/definitions/…;
;;;; title, description, default и format не проверяются. Сначала проверка
;;;; сверяется с примерами из того же репозитория и с намеренно испорченными
;;;; файлами, затем ею проверяется экспорт прогона.

(in-package #:worm.tests)

(def-suite wcon :in worm :description "Экспорт WCON и схема формата.")
(in-suite wcon)

(defun wcon-data (name) (merge-pathnames (format nil "tests/data/wcon/~A" name) (worm.core:project-root)))

(defun json-type-p (type value)
  (cond ((string= type "object") (hash-table-p value))
        ((string= type "array") (and (vectorp value) (not (stringp value))))
        ((string= type "string") (stringp value))
        ((string= type "number") (realp value))
        ((string= type "integer") (integerp value))
        ((string= type "null") (eq value 'null))
        ((string= type "boolean") (or (eq value t) (null value)))
        (t (error "тип ~S схема не знает" type))))

(defun schema-errors (schema value root &optional (path "$"))
  "Список нарушений схемы SCHEMA значением VALUE; ROOT — корень для $ref."
  (let ((errors '()))
    (flet ((violation (control &rest args) (push (format nil "~A: ~?" path control args) errors))
           (key (k) (and (hash-table-p schema) (gethash k schema))))
      (when (hash-table-p schema)
        (let ((ref (key "$ref")))
          (when ref
            (let ((target root))
              (dolist (part (rest (cl-ppcre:split "/" (subseq ref 1))))
                (setf target (gethash part target)))
              (return-from schema-errors (schema-errors target value root path)))))
        (let ((type (key "type")))
          (when (and type (notany (lambda (ty) (json-type-p ty value)) (if (stringp type) (list type) (coerce type 'list))))
            (violation "тип ~A, а не ~A" type (type-of value))
            (return-from schema-errors errors)))
        (let ((enum (key "enum")))
          (when (and enum (not (find value enum :test #'equal)))
            (violation "~S не из ~S" value enum)))
        (when (and (key "minLength") (stringp value) (< (length value) (key "minLength")))
          (violation "строка короче ~D" (key "minLength")))
        (when (and (key "minItems") (vectorp value) (< (length value) (key "minItems")))
          (violation "элементов меньше ~D" (key "minItems")))
        (let ((items (key "items")))
          (when (and items (vectorp value) (not (stringp value)))
            (loop for v across value for i from 0
                  do (setf errors (append (schema-errors items v root (format nil "~A[~D]" path i)) errors)))))
        (when (hash-table-p value)
          (let ((props (key "properties")) (patterns (key "patternProperties"))
                (additional (multiple-value-list (key "additionalProperties"))))
            (when (key "required")
              (loop for r across (key "required")
                    unless (nth-value 1 (gethash r value)) do (violation "нет обязательного ~A" r)))
            (maphash (lambda (k v)
                       (let ((sub (format nil "~A.~A" path k)) (matched nil))
                         (when (and props (nth-value 1 (gethash k props)))
                           (setf matched t errors (append (schema-errors (gethash k props) v root sub) errors)))
                         (when patterns
                           (maphash (lambda (re s)
                                      (when (cl-ppcre:scan re k)
                                        (setf matched t errors (append (schema-errors s v root sub) errors))))
                                    patterns))
                         (when (and (not matched) (second additional))
                           (let ((a (first additional)))
                             (cond ((null a) (violation "лишнее поле ~A" k))
                                   ((hash-table-p a) (setf errors (append (schema-errors a v root sub) errors))))))))
                     value)))
        (let ((one (key "oneOf")))
          (when one
            (let ((ok (count-if (lambda (s) (null (schema-errors s value root path))) one)))
              (unless (= ok 1) (violation "oneOf: подходит ~D вариантов из ~D" ok (length one))))))
        (let ((any (key "anyOf")))
          (when (and any (notany (lambda (s) (null (schema-errors s value root path))) any))
            (violation "anyOf: не подходит ни один вариант")))))
    errors))

(defun wcon-schema () (worm.io:read-json-file (wcon-data "wcon_schema.json")))

(defun wcon-errors (object)
  (let ((schema (wcon-schema))) (schema-errors schema object schema)))

(test schema-check-accepts-the-format-examples
  (dolist (f '("minimal.wcon" "maximal_0.wcon" "multiworm.wcon"))
    (let ((errors (wcon-errors (worm.io:read-json-file (wcon-data f)))))
      (is (null errors) "~A: ~{~A~^; ~}" f errors))))

(test schema-check-rejects-broken-files
  (flet ((broken (text) (wcon-errors (com.inuoe.jzon:parse text))))
    (is (broken "{\"data\":[]}") "без units")
    (is (broken "{\"units\":{\"t\":\"s\",\"x\":\"mm\"},\"data\":[]}") "units без y")
    (is (broken "{\"units\":{\"t\":\"s\",\"x\":\"mm\",\"y\":\"mm\"},\"data\":[{\"id\":\"1\",\"t\":[0],\"x\":\"a\",\"y\":[[1]]}]}")
        "x — строка")
    (is (broken "{\"units\":{\"t\":\"s\",\"x\":\"mm\",\"y\":\"mm\"},\"data\":[{\"id\":\"1\",\"t\":[0],\"x\":[[1]],\"y\":[[1]],\"head\":\"X\"}]}")
        "head не из L, R, ?")
    (is (broken "{\"units\":{\"t\":\"s\",\"x\":\"mm\",\"y\":\"mm\"},\"data\":[{\"t\":[0],\"x\":[[1]],\"y\":[[1]]}]}")
        "запись без id")
    (is (broken "{\"files\":{\"current\":\"a\",\"bogus\":1},\"units\":{\"t\":\"s\",\"x\":\"mm\",\"y\":\"mm\"},\"data\":[]}")
        "лишнее поле в files")))

(test run-exports-valid-wcon
  (let* ((gen (worm.codegen:compile-model 'worm.components:worm-3d :params "agar-a-v1" :rings 8))
         (run (worm.runtime:start-run gen :params "agar-a-v1" :dt 1d-3 :paused t :record t :frames-every 0.01d0
                                          :directory (runtime-dir "wcon"))))
    ;; экспорт идущего прогона: export-wcon сам сбрасывает записанные кадры
    (let* ((path (unwind-protect (progn (worm.runtime:step-run 200 run) (worm:export-wcon run))
                   (worm.runtime:stop-run run)))
           (w (worm.io:read-json-file path))
           (record (aref (gethash "data" w) 0))
           (times (gethash "t" record)) (xs (gethash "x" record)))
      (is (null (wcon-errors w)) "~{~A~^; ~}" (wcon-errors w))
      (is (= 21 (length times)) "кадр каждые 0,01 с: ~D" (length times))
      (is (every #'< times (subseq times 1)) "время растёт")
      (is (= 8 (length (aref xs 0))) "точка средней линии на кольцо")
      (is (< (aref (aref xs 0) 0) (aref (aref xs 0) 7)) "голова — первая точка, ось x к хвосту")
      (is (string= "L" (gethash "head" record))))))

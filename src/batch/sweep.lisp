;;;; src/batch/sweep.lisp — конфигурации прогонов, развёртки и задания.
;;;;
;;;; Архитектура, «Пакетные прогоны». Конфигурация прогона (configs/*.sexp):
;;;;
;;;;   (config <имя>
;;;;     :model worm-3d-proprio        ; модель defmodel (по имени символа)
;;;;     :params "agar-a-v1"           ; набор параметров: имя или путь
;;;;     :medium nil :rings nil        ; необязательно
;;;;     :dt 1d-3 :duration 10         ; шаг и длительность, с
;;;;     :neural-every 10              ; R — пересборка нервной матрицы, шагов
;;;;     :set ((sr-scale 1.4) (tau-ca 120 :ms))   ; правки, единицы реестра
;;;;     :record (:frames-every 0.01)  ; запись кадров
;;;;     :metrics-from 4               ; метрики походки с этого момента, с
;;;;     :timeout 1800)                ; предел задания, с
;;;;
;;;; Развёртка (sweeps/*.sexp):
;;;;
;;;;   (sweep <имя>
;;;;     :base #p"configs/proprio-agar.sexp"
;;;;     :vary ((viscosity :geometric 1 10000 :steps 5 :multiply t :params (c-t c-n))
;;;;            (sr-scale :list (1 1.4 2)))
;;;;     :design :grid                 ; или (:latin-hypercube :samples 16 :seed 1)
;;;;     :duration 8 :timeout 600 :workers :auto)
;;;;
;;;; Изменение :geometric и :linear — от и до за :steps значений, :list —
;;;; перечень. :params — какие параметры меняет изменение (по умолчанию —
;;;; одноимённый); :multiply t — значение множит базовое. Файлы читаются с
;;;; *read-eval* = nil.

(in-package #:worm.batch)

(define-condition batch-error (worm-error) ())

(defun batch-error (control &rest args)
  (error 'batch-error :message (apply #'format nil control args)))

(defun read-form (path)
  "Первая форма файла PATH; символы — в пакете WORM.BATCH."
  (with-open-file (in path :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*read-eval* nil) (*package* (find-package :worm.batch))
            (*read-default-float-format* 'double-float))
        (read in)))))

(defun kw (x) (if (keywordp x) x (intern (string-upcase (string x)) :keyword)))

(defun project-path (designator)
  (let ((p (pathname designator)))
    (if (uiop:absolute-pathname-p p) p (merge-pathnames p (project-root)))))

;;; Конфигурация

(defun normalize-set (entries)
  "((имя значение [единица]) …) → ((:имя значение единица-или-NIL) …):
вещественные — double-float, целые и выбор (:anterior) — как есть."
  (mapcar (lambda (e)
            (destructuring-bind (name value &optional unit) e
              (list (kw name)
                    (cond ((integerp value) value)
                          ((realp value) (float value 0d0))
                          ((symbolp value) (kw value))
                          (t (batch-error ":set: значение ~S у ~(~A~)" value name)))
                    (and unit (kw unit)))))
          entries))

(defun load-config (path)
  "Конфигурация прогона из файла: plist."
  (let ((form (read-form (project-path path))))
    (unless (and (consp form) (string= (symbol-name (first form)) "CONFIG"))
      (batch-error "~A: ожидалась форма (config <имя> …)" path))
    (destructuring-bind (&key model (params "water-v1") medium rings (dt 1d-3) (neural-every 10) (duration 1) set
                           (record '(:frames-every 0.01d0)) (metrics-from 0) (timeout 3600))
        (cddr form)
      (unless model (batch-error "~A: нет :model" path))
      (list :name (string-downcase (string (second form)))
            :model (string-upcase (string model))
            :params (if (pathnamep params) (namestring params) params)
            :medium (and medium (kw medium)) :rings rings
            :dt (float dt 0d0) :neural-every neural-every :duration (float duration 0d0)
            :set (normalize-set set) :record record
            :metrics-from (float metrics-from 0d0) :timeout timeout))))

;;; Развёртка

(defstruct (sweep (:copier nil))
  name base vary design duration timeout workers neural-every path)

(defun load-sweep (path)
  (let ((form (read-form (project-path path))))
    (unless (and (consp form) (string= (symbol-name (first form)) "SWEEP"))
      (batch-error "~A: ожидалась форма (sweep <имя> …)" path))
    (destructuring-bind (&key base vary (design :grid) duration timeout (workers :auto) neural-every) (cddr form)
      (unless base (batch-error "~A: нет :base" path))
      (make-sweep :name (string-downcase (string (second form)))
                  :base (load-config base)
                  :vary (mapcar #'parse-vary vary)
                  :design design :duration duration :timeout timeout :workers workers :neural-every neural-every
                  :path (project-path path)))))

(defun parse-vary (entry)
  "(имя вид … &key steps multiply params) → plist (:name :values :multiply :params)."
  (destructuring-bind (name kind &rest args) entry
    (flet ((options (rest) (if (and rest (keywordp (first rest))) rest nil)))
      (let* ((kind (kw kind))
             (values
               (ecase kind
                 (:list (mapcar (lambda (v) (float v 0d0)) (first args)))
                 ((:geometric :linear)
                  (destructuring-bind (lo hi &key (steps 2) &allow-other-keys) args
                    (loop for k below steps
                          for f = (if (= steps 1) 0 (/ k (1- steps)))
                          collect (float (if (eq kind :geometric)
                                             (* lo (expt (/ hi lo) f))
                                             (+ lo (* f (- hi lo))))
                                         0d0))))))
             (opts (options (if (eq kind :list) (rest args) (cddr args)))))
        (list :name (kw name) :values values :kind kind
              :multiply (getf opts :multiply)
              :params (mapcar #'kw (or (getf opts :params) (list name))))))))

(defun grid (vary)
  (if (null vary) (list '())
      (loop for v in (getf (first vary) :values)
            append (mapcar (lambda (rest) (cons (list (getf (first vary) :name) v) rest))
                           (grid (rest vary))))))

(defun latin-hypercube (vary samples seed)
  "Выборка латинского гиперкуба: по каждой оси SAMPLES слоёв, случайная перестановка."
  (let ((state (sb-ext:seed-random-state seed)))
    (let ((columns
            (loop for v in vary
                  collect (let* ((vals (getf v :values))
                                 (perm (let ((p (coerce (loop for i below samples collect i) 'vector)))
                                         (loop for i from (1- samples) downto 1
                                               do (rotatef (aref p i) (aref p (random (1+ i) state))))
                                         p)))
                            (loop for i below samples
                                  collect (let ((u (/ (+ (aref perm i) (random 1d0 state)) samples)))
                                            (if (eq (getf v :kind) :list)
                                                (nth (min (1- (length vals)) (floor (* u (length vals)))) vals)
                                                (let ((lo (first vals)) (hi (car (last vals))))
                                                  (if (eq (getf v :kind) :geometric)
                                                      (* lo (expt (/ hi lo) u))
                                                      (+ lo (* u (- hi lo))))))))))))
      (loop for i below samples
            collect (loop for v in vary for col in columns
                          collect (list (getf v :name) (float (nth i col) 0d0)))))))

(defun design-points (sweep)
  (let ((design (sweep-design sweep)))
    (cond ((eq design :grid) (grid (sweep-vary sweep)))
          ((and (consp design) (eq (first design) :latin-hypercube))
           (destructuring-bind (&key (samples 8) (seed 1)) (rest design)
             (latin-hypercube (sweep-vary sweep) samples seed)))
          (t (batch-error "развёртка ~A: неизвестный план ~S" (sweep-name sweep) design)))))

;;; Задания

(defun base-value (config name)
  "Значение параметра NAME в единицах реестра: из :set конфигурации или из набора."
  (let ((entry (assoc name (getf config :set))))
    (if entry
        (second entry)
        (let* ((set (worm.params:load-param-set (getf config :params)))
               (spec (worm.params:find-parameter name))
               (unit (worm.params:parameter-spec-unit spec))
               (v (worm.params:param-value set name)))
          (if unit (worm.params:from-internal v unit) (float v 0d0))))))

(defparameter +numeric-keys+ '(:dt :neural-every)
  "Изменения развёртки с этими именами меняют не параметр модели, а шаг h
или R задания (сходимость по шагу).")

(defun resolve-point (sweep point)
  "Правки :set задания для точки POINT развёртки."
  (let ((set (copy-tree (getf (sweep-base sweep) :set))))
    (loop for (name value) in point
          for v = (find name (sweep-vary sweep) :key (lambda (x) (getf x :name)))
          unless (member name +numeric-keys+)
          do (dolist (param (getf v :params))
               (let ((new (if (getf v :multiply) (* value (base-value (sweep-base sweep) param)) value)))
                 (setf set (cons (list param new nil) (remove param set :key #'first))))))
    (sort set #'string< :key (lambda (e) (symbol-name (first e))))))

(defun model-identity (name)
  "Хэш модели и версия кодогенератора: другая модель или другой код шага —
другой прогон, даже при той же конфигурации."
  (let ((symbol (find-symbol name :worm.components)))
    (list (and symbol (worm.dsl:model-spec-hash (worm.dsl:find-model symbol)))
          worm.codegen:+codegen-version+)))

(defun canonical-text (job)
  "Текст задания без имени развёртки и идентификатора: от него берётся хэш."
  (with-standard-io-syntax
    (let ((*print-readably* nil) (*print-case* :downcase))
      (prin1-to-string (append (loop for key in '(:model :params :medium :rings :dt :duration :set :record :metrics-from)
                                     collect key collect (getf job key))
                               ;; R по умолчанию в текст не входит: задания до этапа 12 сохраняют идентификаторы
                               (let ((r (getf job :neural-every 10))) (unless (eql r 10) (list :neural-every r)))
                               (list :code (model-identity (getf job :model))))))))

(defun job-id (job)
  (subseq (worm.platform:md5-hex (canonical-text job)) 0 16))

(defun expand-sweep (sweep)
  "Задания развёртки: список plist с :id."
  (let ((base (sweep-base sweep)))
    (loop for point in (design-points sweep)
          collect (let* ((numeric (lambda (key default)
                                    (let ((p (assoc key point)))
                                      (if p (if (eq key :neural-every) (round (second p)) (second p)) default))))
                         (job (list :sweep (sweep-name sweep)
                                   :model (getf base :model) :params (getf base :params)
                                   :medium (getf base :medium) :rings (getf base :rings)
                                   :dt (funcall numeric :dt (getf base :dt))
                                   :neural-every (funcall numeric :neural-every
                                                          (or (sweep-neural-every sweep) (getf base :neural-every 10)))
                                   :duration (float (or (sweep-duration sweep) (getf base :duration)) 0d0)
                                   :set (resolve-point sweep point)
                                   :record (getf base :record)
                                   :metrics-from (getf base :metrics-from)
                                   :timeout (or (sweep-timeout sweep) (getf base :timeout))
                                   :point point)))
                    (list* :id (job-id job) job)))))

(defun jobs-directory (sweep-name)
  (merge-pathnames (format nil "jobs/~A/" sweep-name) (project-root)))

(defun job-path (job) (merge-pathnames (format nil "~A.sexp" (getf job :id)) (jobs-directory (getf job :sweep))))

(defun write-job (job)
  (let ((path (job-path job)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
      (with-standard-io-syntax
        (let ((*print-readably* nil) (*print-case* :downcase) (*print-pretty* t) (*print-right-margin* 100))
          (format out ";;;; Задание развёртки ~A: полная разрешённая конфигурация.~%" (getf job :sweep))
          (prin1 (cons 'job job) out)
          (terpri out))))
    path))

(defun read-job (path)
  (let ((form (read-form path)))
    (unless (and (consp form) (string= (symbol-name (first form)) "JOB"))
      (batch-error "~A: ожидалась форма (job …)" path))
    (rest form)))

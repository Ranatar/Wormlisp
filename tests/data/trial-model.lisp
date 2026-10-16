;;;; tests/data/trial-model.lisp — пробная модель на языке модели (этап 6).
;;;;
;;;; Кутикула на всех четырёх семействах рёбер с предварительными
;;;; натяжениями, давление отсеков, кинематическая мышца (заданная волна
;;;; натяжения) и кальций мышцы. Механическая часть повторяет ручную модель
;;;; tests/oracle/ (кинематическое натяжение, p₀), поэтому интерпретатор
;;;; сверяется с ней. Файл загружается и тестами, и отдельным процессом SBCL
;;;; для сверки канонического текста между запусками.

(in-package #:cl-user)

(macrolet ((arrays (&rest names)
             `(progn
                ,@(loop for name in names
                        collect `(worm.dsl:define-element-array ,name
                                   :family :edge
                                   :compute (lambda (context)
                                              (make-array (getf context :count) :element-type 'double-float
                                                                                :initial-element 0d0))
                                   :documentation "Пробная модель: значения задаются при сборке.")))))
  (arrays trial-k-long trial-k-circ trial-k-diag trial-k-diam trial-eta-long trial-eta-circ trial-eta-diag trial-eta-diam
          trial-t-long trial-t-circ trial-t-diag trial-t-diam))

(worm.dsl:define-component trial-cuticle-edge
  :family :edge
  :params ((stiffness :per-element) (viscosity :per-element) (rest-tension :per-element))
  :inputs ((tension (link :muscle-tension) :default 0))
  :let ((u (unit edge-vector))
        (f (+ (* stiffness (- edge-length rest-length)) rest-tension tension)))
  :forces (((pos a) (scale f u))
           ((pos b) (scale (- f) u)))
  :damping ((viscosity length-rate))
  :documentation "Ребро кутикулы: упругость, натяжение покоя, натяжение мышц, вязкость.")

(worm.dsl:define-component trial-pressure
  :family :compartment
  :params (bulk-modulus prestress)
  :let ((p (+ prestress (* bulk-modulus (/ (- rest-volume volume) rest-volume)))))
  :forces ((:gradient volume :scale p))
  :documentation "Давление отсека p₀ + K·(V⁰ − V)/V⁰, сила p·∂V/∂x.")

(worm.dsl:define-component trial-kinematic-muscle
  :family :muscle
  :params (gen-amplitude gen-frequency gen-wavelength)
  :outputs ((force (* gen-amplitude
                      (positive (* side (sin (* 2 pi (- (* gen-frequency (time))
                                                         (/ position gen-wavelength)))))))))
  :documentation "Кинематическая мышца: заданная бегущая волна натяжения.")

(worm.dsl:define-component trial-calcium
  :family :muscle
  :params (tau-ca ca-half hill-n)
  :state ((ca 0))
  :inputs ((drive (link :drive)))
  :derivs ((ca (/ (- (positive drive) ca) tau-ca)))
  :outputs ((activation (hill ca ca-half hill-n)))
  :documentation "Кальций мышцы под входом и активация по Хиллу.")

(worm.dsl:defmodel trial-worm
  :topology (tube :rings 50 :ring-nodes 8 :length body-length
                  :radius-max radius-max :radius-end-min radius-end-min)
  :anatomy "anatomy/muscles.sexp"
  :components ((cuticle-long trial-cuticle-edge :on :long-edge
                             :rename (stiffness trial-k-long viscosity trial-eta-long rest-tension trial-t-long))
               (cuticle-circ trial-cuticle-edge :on :circ-edge
                             :rename (stiffness trial-k-circ viscosity trial-eta-circ rest-tension trial-t-circ))
               (shear trial-cuticle-edge :on :diagonal
                      :rename (stiffness trial-k-diag viscosity trial-eta-diag rest-tension trial-t-diag))
               (shape trial-cuticle-edge :on :diametral
                      :rename (stiffness trial-k-diam viscosity trial-eta-diam rest-tension trial-t-diam))
               (pressure trial-pressure :on :compartment)
               (drive trial-kinematic-muscle :on :muscle)
               (calcium trial-calcium :on :muscle))
  :wiring (((drive force) (cuticle-long tension) :via :attachment)
           ((drive force) (calcium drive) :via :identity))
  :subsystems ((:mechanical cuticle-long cuticle-circ shear shape pressure drive)
               (:neural calcium)))

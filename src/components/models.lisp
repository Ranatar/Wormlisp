;;;; src/components/models.lisp — модели червя.
;;;;
;;;;   worm-3d            мышцы под генератором ритма уровня 1 (этап 5)
;;;;   worm-3d-kinematic  кинематическое натяжение мышц (этап 3): генератор
;;;;                      ритма с амплитудой в мкН прямо тянет продольные рёбра

(in-package #:worm.components)

(defmodel worm-3d
  :topology (tube :rings 50 :ring-nodes 8 :length body-length
                  :radius-max radius-max :radius-end-min radius-end-min)
  :anatomy "anatomy/muscles.sexp"
  :components ((cuticle-long cuticle-edge :on :long-edge
                             :rename (stiffness k-long viscosity eta-long rest-tension t-long))
               (cuticle-circ cuticle-edge :on :circ-edge
                             :rename (stiffness k-circ viscosity eta-circ rest-tension t-circ))
               (shear cuticle-edge :on :diagonal
                      :rename (stiffness k-diag viscosity eta-diag rest-tension t-diag))
               (shape cuticle-edge :on :diametral
                      :rename (stiffness k-diam viscosity eta-diam rest-tension t-diam))
               (pressure hydrostatic :on :compartment)
               (contact substrate :on :node :when (medium :agar))
               (drag rft :on :node)
               (generator rhythm-generator :on :muscle)
               (muscles rate-muscle :on :muscle
                        :rename (f-max muscle-f-max lambda-min muscle-lambda-min)))
  :wiring (((generator drive) (muscles drive) :via :identity)
           ((muscles force) (cuticle-long tension) :via :attachment)
           ((cuticle-long length) (muscles attached-length) :via :attachment)
           ((cuticle-long rest) (muscles attached-rest) :via :attachment)
           ((cuticle-long rate) (muscles attached-rate) :via :attachment))
  :subsystems ((:mechanical cuticle-long cuticle-circ shear shape pressure contact drag)
               (:neural generator muscles)))

(defmodel worm-3d-kinematic
  :topology (tube :rings 50 :ring-nodes 8 :length body-length
                  :radius-max radius-max :radius-end-min radius-end-min)
  :anatomy "anatomy/muscles.sexp"
  :components ((cuticle-long cuticle-edge :on :long-edge
                             :rename (stiffness k-long viscosity eta-long rest-tension t-long))
               (cuticle-circ cuticle-edge :on :circ-edge
                             :rename (stiffness k-circ viscosity eta-circ rest-tension t-circ))
               (shear cuticle-edge :on :diagonal
                      :rename (stiffness k-diag viscosity eta-diag rest-tension t-diag))
               (shape cuticle-edge :on :diametral
                      :rename (stiffness k-diam viscosity eta-diam rest-tension t-diam))
               (pressure hydrostatic :on :compartment)
               (contact substrate :on :node :when (medium :agar))
               (drag rft :on :node)
               (tension rhythm-generator :on :muscle :rename (gen-amplitude tension-amplitude)))
  :wiring (((tension drive) (cuticle-long tension) :via :attachment))
  :subsystems ((:mechanical cuticle-long cuticle-circ shear shape pressure contact drag)
               (:neural tension)))

;;;; Третий уровень с рецептивным полем вперёд (Wen 2012), этап 12: вода и
;;;; вязкая вода (×10⁴).
(sweep connectome-anterior
  :base #p"configs/connectome-water-anterior.sexp"
  :vary ((viscosity :list (1 10000) :multiply t :params (c-t c-n)))
  :duration 8
  :timeout 7200)

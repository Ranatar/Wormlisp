;;;; Уровень 1 в воде (этап 14): та же сила мышц, что на агаре в
;;;; rhythm-agar-amplitude, — зависит ли амплитуда кривизны от среды при
;;;; заданном входе мышц.
(sweep rhythm-water-amplitude
  :base #p"configs/rhythm-water.sexp"
  :vary ((f-max-mid :list (16.5 33 48)))
  :design :grid
  :timeout 3600)

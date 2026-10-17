;;;; Частотная характеристика тела в воде (план, этап 15): уровень 1 с волной
;;;; λ = 1,5 L на разных частотах — та же, что у тела Бойла с заданным входом
;;;; мышц (scripts/boyle-wormsim.sh, FORCE_F).
(sweep forced-water
  :base #p"configs/rhythm-water.sexp"
  :vary ((gen-frequency :list (0.5 1 2 4))
         (gen-wavelength :list (1.5))
         (f-max-mid :list (16.5 48)))
  :design :grid
  :duration 6
  :timeout 3600)

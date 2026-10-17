;;;; То же, что boyle-real, с нервной частью из кода Бойла: поле 0,5 L,
;;;; усиление рецептора 0,46, сглаживание 50 мс (план, этап 15).
(sweep boyle-real-neural
  :base #p"configs/boyle-compare.sexp"
  :vary ((medium :list (0 0.001 0.01 0.1 1)
                 :interpolate ((c-t 3.3e-3 46) (c-n 5.2e-3 280)))
         (bending-ei :list (0.095))
         (f-max-mid :list (7.1))
         (rf-length :list (0.5))
         (sr-scale :list (0.46))
         (sr-filter :list (50)))
  :design :grid
  :timeout 3600)

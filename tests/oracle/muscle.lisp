;;;; tests/oracle/muscle.lisp — усреднённая мышца (дизайн, «Модель мышцы»).
;;;;
;;;; Вход → кальций:  τ_Ca·dCa/dt = Ca∞(I) − Ca,  Ca∞ = 1/(1 + e^{−(I − I½)/σ});
;;;; кальций → активация: a = Caⁿ/(Caⁿ + K_Caⁿ);
;;;; активация → натяжение: T = F_max·a·f_L(λ)·f_V(λ̇).
;;;;
;;;; Кальций нормирован (Ca_max = 1), вход I уровней 1 и 2 — тоже: 1 —
;;;; полный вход. Кальций живёт в нервно-мышечной подсистеме (y-neural), а
;;;; множители f_L и f_V берутся по длине и скорости мышцы в начале шага и
;;;; в пределах механического шага постоянны (архитектура, решение 22).
;;;;
;;;; Длина мышцы λ — средняя по её рёбрам с весами привязки, в долях длины
;;;; покоя; скорость λ̇ — по скоростям узлов прошлого шага (aux). f_V читает
;;;; не саму λ̇, а сглаженную: τ_v·dλ̄̇/dt = λ̇ − λ̄̇, ещё одно состояние клетки
;;;; в y-neural. Без сглаживания запаздывающая на шаг обратная связь по
;;;; скорости расходится, когда демпфирование мышцы F_max·(1 + 1/a)/v_max
;;;; больше вязкости тканей (отношение ρ > 1), при любом h; со сглаживанием
;;;; шаг устойчив при h < 2·τ_v/(ρ − 1).
;;;;
;;;; f_L — колокол с максимумом при длине покоя, падающий до нуля при
;;;; предельном укорочении λ_min = 1 − Δ_M·r/R_max (Boyle et al. 2012):
;;;;   f_L = max(0, 1 − ((λ − 1)/(1 − λ_min))²).
;;;; f_V — гипербола Хилла при укорочении (u = −λ̇/v_max ≥ 0):
;;;;   f_V = max(0, (1 − u)/(1 + u/a)),
;;;; и насыщение до E при удлинении (w = −u > 0) с тем же наклоном в нуле:
;;;;   f_V = 1 + (E − 1)·w/(w + c),  c = (E − 1)·a/(1 + a).

(in-package #:worm.oracle)

(declaim (inline ca-steady hill-activation force-length force-velocity))

(defun ca-steady (input half width)
  "Ca∞(I) — стационарный кальций при входе INPUT."
  (declare (type double-float input half width))
  (/ 1d0 (+ 1d0 (exp (- (/ (- input half) width))))))

(defun hill-activation (ca k n)
  "a = Caⁿ/(Caⁿ + Kⁿ); отрицательный кальций (округление) считается нулём."
  (declare (type double-float ca k n))
  (if (<= ca 0d0)
      0d0
      ;; xⁿ через exp и log: expt с вещественным показателем упаковывает
      (let ((x (exp (* n (log (the (double-float (0d0)) (/ ca k)))))))
        (declare (type double-float x))
        (/ x (+ 1d0 x)))))

(defun force-length (lam lam-min)
  "f_L(λ): 1 при λ = 1, 0 при λ = λ_min и при симметричном перерастяжении."
  (declare (type double-float lam lam-min))
  (let ((x (/ (- lam 1d0) (- 1d0 lam-min))))
    (max 0d0 (- 1d0 (* x x)))))

(defun force-velocity (rate v-max a eccentric)
  "f_V по скорости изменения длины RATE (1/с, отрицательна при укорочении)."
  (declare (type double-float rate v-max a eccentric))
  (let ((u (/ (- rate) v-max)))
    (if (>= u 0d0)
        (max 0d0 (/ (- 1d0 u) (+ 1d0 (/ u a))))
        (let ((w (- u)) (c (/ (* (- eccentric 1d0) a) (+ 1d0 a))))
          (+ 1d0 (/ (* (- eccentric 1d0) w) (+ w c)))))))

;;; Поклеточные величины

(defun muscle-f-max (muscles set)
  "F_max каждой клетки: F_max середины × линейный градиент от головы к хвосту
(нормирован на середину тела) × ослабление первой клетки каждого ряда."
  (let* ((mid (param set :f-max-mid))
         (grad (worm.params:param-value set :f-max-gradient))
         (g0 (float (aref grad 0) 0d0)) (g1 (float (aref grad 1) 0d0))
         (g-mid (* 0.5d0 (+ g0 g1)))
         (weak (param set :head-weakening)))
    (map 'dvec (lambda (m)
                 (let* ((s (worm.anatomy:muscle-peak m))
                        (g (+ g0 (* (- g1 g0) s)))
                        (first-in-row (<= (worm.anatomy:muscle-index m) 2)))
                   (* mid (/ g g-mid) (if first-in-row weak 1d0))))
         muscles)))

(defun muscle-lambda-min (muscles tube max-shortening)
  "λ_min каждой клетки: 1 − Δ_M·r/R_max, r — радиус тела у её максимума."
  (let* ((frac (worm.topology:tube-ring-fractions tube))
         (radius (worm.topology:tube-ring-radius tube))
         (r-max (worm.topology:tube-radius-max tube))
         (rings (length frac)))
    (map 'dvec (lambda (m)
                 (let* ((s (worm.anatomy:muscle-peak m))
                        (i (min (- rings 2) (max 0 (floor (* s (1- rings))))))
                        (u (/ (- s (aref frac i)) (- (aref frac (1+ i)) (aref frac i))))
                        (r (+ (* (- 1d0 u) (aref radius i)) (* u (aref radius (1+ i))))))
                   (- 1d0 (/ (* max-shortening r) r-max))))
         muscles)))

(defun muscle-rest-lengths (binding long-rest)
  "Σ w·ℓ₀ по рёбрам каждой клетки — знаменатель её относительной длины."
  (let* ((offsets (worm.anatomy:muscle-binding-offsets binding))
         (edges (worm.anatomy:muscle-binding-edges binding))
         (weights (worm.anatomy:muscle-binding-weights binding))
         (n (1- (length offsets)))
         (out (make-dvec n)))
    (dotimes (m n out)
      (loop for q from (aref offsets m) below (aref offsets (1+ m))
            do (incf (aref out m) (* (aref weights q) (aref long-rest (aref edges q))))))))

(defun muscle-geometry (y aux offsets edges weights ea eb rest-sum lam rate)
  "Относительная длина λ и её скорость λ̇ (1/с) каждой клетки: средние по её
рёбрам с весами привязки. Скорости узлов — AUX (прошлый шаг) или NIL."
  (declare (type dvec y rest-sum lam rate weights) (type (or null dvec) aux)
           (type ivec offsets edges ea eb) (optimize (speed 3) (safety 1)))
  (dotimes (m (length rest-sum))
    (let ((len 0d0) (dlen 0d0))
      (declare (type double-float len dlen))
      (loop for q of-type fixnum from (aref offsets m) below (aref offsets (1+ m))
            do (let* ((e (aref edges q)) (w (aref weights q))
                      (pa (* 3 (aref ea e))) (pb (* 3 (aref eb e)))
                      (dx (- (aref y pb) (aref y pa)))
                      (dy (- (aref y (+ pb 1)) (aref y (+ pa 1))))
                      (dz (- (aref y (+ pb 2)) (aref y (+ pa 2))))
                      (l (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
                 (declare (type fixnum e pa pb) (type double-float w dx dy dz l))
                 (incf len (* w l))
                 (when aux
                   (incf dlen (* w (/ (+ (* dx (- (aref aux pb) (aref aux pa)))
                                         (* dy (- (aref aux (+ pb 1)) (aref aux (+ pa 1))))
                                         (* dz (- (aref aux (+ pb 2)) (aref aux (+ pa 2)))))
                                      l))))))
      (setf (aref lam m) (/ len (aref rest-sum m))
            (aref rate m) (/ dlen (aref rest-sum m)))))
  (values))


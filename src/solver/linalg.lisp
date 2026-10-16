;;;; src/solver/linalg.lisp — ленточная и плотная LU на Лиспе.
;;;;
;;;; Архитектура, решение 16: своя LU вместо LAPACK — код виден
;;;; компилятору, нет внешней зависимости, результат детерминирован.
;;;;
;;;; Ленточная LU — перевод LAPACK dgbtf2 (без блоков) и dgbtrs на индексы
;;;; с нуля. Хранение ленты описано в worm.core (band-index): матрица
;;;; несимметрична, поэтому ведущий элемент выбирается по столбцу, а верхняя
;;;; лента расширяется на kl строк заполнения.
;;;;
;;;; Функции не выделяют память: все массивы передаются снаружи.

(in-package #:worm.solver)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun band-factor! (ab n kl ku ipiv)
  "LU-разложение ленты AB (n × n, kl под- и ku наддиагоналей) на месте.
IPIV получает перестановки. Возвращает NIL при успехе или номер столбца
(с нуля), где ведущий элемент оказался нулём."
  (declare (type dvec ab) (type ivec ipiv) (type (unsigned-byte 31) n kl ku))
  (let* ((kv (+ kl ku))
         (ldab (+ kv kl 1))
         (ju 0)
         (info nil))
    (declare (type (unsigned-byte 31) kv ldab ju))
    ;; обнулить заполнение в столбцах ku+1 … min(kv, n)−1
    (loop for j of-type (unsigned-byte 31) from (1+ ku) below (min kv n)
          do (loop for r of-type (unsigned-byte 31) from (- kv j) below kl
                   do (setf (aref ab (+ r (* j ldab))) 0d0)))
    (dotimes (j n)
      (declare (type (unsigned-byte 31) j))
      ;; обнулить заполнение в столбце j + kv
      (when (< (+ j kv) n)
        (dotimes (r kl)
          (setf (aref ab (+ r (* (+ j kv) ldab))) 0d0)))
      (let* ((km (min kl (- n 1 j)))
             (col (* j ldab))
             (jp 0)
             (best (abs (aref ab (+ kv col)))))
        (declare (type (unsigned-byte 31) km col jp) (type double-float best))
        ;; ведущий элемент — наибольший по модулю в столбце под диагональю
        (loop for p of-type (unsigned-byte 31) from 1 to km
              do (let ((v (abs (aref ab (+ kv p col)))))
                   (when (> v best) (setf best v jp p))))
        (setf (aref ipiv j) (+ jp j))
        (if (zerop (aref ab (+ kv jp col)))
            (unless info (setf info j))
            (progn
              (setf ju (max ju (min (+ j ku jp) (- n 1))))
              ;; перестановка строк j и j+jp в столбцах j … ju
              (when (/= jp 0)
                (loop for c of-type (unsigned-byte 31) from 0 to (- ju j)
                      for x of-type (unsigned-byte 31) = (+ kv jp (- c) (* (+ j c) ldab))
                      for y of-type (unsigned-byte 31) = (+ kv (- c) (* (+ j c) ldab))
                      do (rotatef (aref ab x) (aref ab y))))
              (when (> km 0)
                ;; множители
                (let ((inv (/ 1d0 (aref ab (+ kv col)))))
                  (loop for p of-type (unsigned-byte 31) from 1 to km
                        do (setf (aref ab (+ kv p col)) (* (aref ab (+ kv p col)) inv))))
                ;; обновление оставшейся части в пределах ленты
                (loop for c of-type (unsigned-byte 31) from 1 to (- ju j)
                      do (let* ((ccol (* (+ j c) ldab))
                                (yv (aref ab (+ (- kv c) ccol))))
                           (declare (type (unsigned-byte 31) ccol) (type double-float yv))
                           (unless (zerop yv)
                             (loop for p of-type (unsigned-byte 31) from 1 to km
                                   do (decf (aref ab (+ (- (+ kv p) c) ccol))
                                            (* (aref ab (+ kv p col)) yv)))))))))))
    info))

(defun band-solve! (ab n kl ku ipiv b)
  "Решить A·x = B по разложению из BAND-FACTOR!; ответ — в B."
  (declare (type dvec ab b) (type ivec ipiv) (type (unsigned-byte 31) n kl ku))
  (let* ((kv (+ kl ku))
         (ldab (+ kv kl 1)))
    (declare (type (unsigned-byte 31) kv ldab))
    ;; L·y = P·b
    (when (> kl 0)
      (dotimes (j (1- n))
        (declare (type (unsigned-byte 31) j))
        (let ((lm (min kl (- n 1 j)))
              (l (aref ipiv j))
              (col (* j ldab)))
          (declare (type (unsigned-byte 31) lm l col))
          (when (/= l j) (rotatef (aref b l) (aref b j)))
          (let ((bj (aref b j)))
            (unless (zerop bj)
              (loop for p of-type (unsigned-byte 31) from 1 to lm
                    do (decf (aref b (+ j p)) (* (aref ab (+ kv p col)) bj))))))))
    ;; U·x = y, у U до kv наддиагоналей
    (loop for j of-type fixnum from (1- n) downto 0
          do (let ((col (* j ldab)))
               (declare (type (unsigned-byte 31) col))
               (unless (zerop (aref b j))
                 (setf (aref b j) (/ (aref b j) (aref ab (+ kv col))))
                 (let ((temp (aref b j)))
                   (loop for i of-type fixnum from (max 0 (- j kv)) below j
                         do (decf (aref b i) (* temp (aref ab (+ (- (+ kv i) j) col)))))))))
    b))

(defun dense-factor! (a n ipiv)
  "LU-разложение плотной матрицы A (n × n) на месте с частичным выбором
ведущего элемента. Возвращает NIL или номер столбца с нулевым ведущим."
  (declare (type dmat a) (type ivec ipiv) (type (unsigned-byte 31) n))
  (let ((info nil))
    (dotimes (j n)
      (declare (type (unsigned-byte 31) j))
      (let ((p j) (best (abs (aref a j j))))
        (declare (type (unsigned-byte 31) p) (type double-float best))
        (loop for i of-type (unsigned-byte 31) from (1+ j) below n
              do (let ((v (abs (aref a i j))))
                   (when (> v best) (setf best v p i))))
        (setf (aref ipiv j) p)
        (when (/= p j)
          (dotimes (c n) (rotatef (aref a j c) (aref a p c))))
        (if (zerop (aref a j j))
            (unless info (setf info j))
            (let ((inv (/ 1d0 (aref a j j))))
              (loop for i of-type (unsigned-byte 31) from (1+ j) below n
                    do (let ((m (* (aref a i j) inv)))
                         (setf (aref a i j) m)
                         (unless (zerop m)
                           (loop for c of-type (unsigned-byte 31) from (1+ j) below n
                                 do (decf (aref a i c) (* m (aref a j c)))))))))))
    info))

(defun dense-solve! (a n ipiv b)
  "Решить A·x = B по разложению из DENSE-FACTOR!; ответ — в B."
  (declare (type dmat a) (type ivec ipiv) (type dvec b) (type (unsigned-byte 31) n))
  (dotimes (j n)
    (let ((p (aref ipiv j)))
      (when (/= p j) (rotatef (aref b j) (aref b p)))))
  (dotimes (i n)
    (let ((s (aref b i)))
      (dotimes (c i) (decf s (* (aref a i c) (aref b c))))
      (setf (aref b i) s)))
  (loop for i of-type fixnum from (1- n) downto 0
        do (let ((s (aref b i)))
             (loop for c of-type fixnum from (1+ i) below n
                   do (decf s (* (aref a i c) (aref b c))))
             (setf (aref b i) (/ s (aref a i i)))))
  b)

(defun band-multiply (ab n kl ku x out)
  "OUT := A·X для неразложенной ленты AB. Нужна тестам и проверкам."
  (declare (type dvec ab x out) (type (unsigned-byte 31) n kl ku))
  (let ((ldab (+ kl kl ku 1)) (kv (+ kl ku)))
    (dotimes (i n out)
      (let ((s 0d0))
        (declare (type double-float s))
        (loop for j of-type fixnum from (max 0 (- i kl)) to (min (1- n) (+ i ku))
              do (incf s (* (aref ab (+ (- (+ kv i) j) (* j ldab))) (aref x j))))
        (setf (aref out i) s)))))

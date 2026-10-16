;;;; scripts/build-image.lisp — сохранить образ worm-image.core.
;;;;
;;;; Образ содержит загруженную систему worm со всеми зависимостями, поэтому
;;;; REPL и рабочие процессы стартуют за доли секунды. Настройку ASDF образ
;;;; не хранит: при запуске снова загружается scripts/init.lisp, и если
;;;; исходники изменились после сборки образа, ASDF перекомпилирует только их.

(asdf:load-system "worm")
(let ((core (merge-pathnames "worm-image.core" (uiop:symbol-call :worm.core :project-root))))
  (format t "~&Сохраняю ~A~%" (namestring core))
  ;; журнал процесса откроется заново в процессе из образа
  (uiop:symbol-call :worm.core :close-log)
  (sb-ext:save-lisp-and-die core :executable nil))

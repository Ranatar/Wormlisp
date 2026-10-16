;;;; tests/runtime-repl.lisp — прогон из терминального REPL в tmux (этап 8,
;;;; make test-full).
;;;;
;;;; Так работают из PuTTY: SBCL в сессии tmux, команды набираются в REPL.
;;;; Тест запускает REPL проекта в отдельной сессии tmux, «набирает» в нём
;;;; команды через send-keys и читает экран через capture-pane: запуск
;;;; прогона, сбой, (problem) и (choose …). Разрыв SSH изображает клиент
;;;; tmux в псевдотерминале (script), убитый сигналом HUP, как убивает его
;;;; sshd при обрыве связи: сессия и счёт должны продолжаться.
;;;; Без tmux и script тест пропускается.

(in-package #:worm.tests)

(in-suite physics)

(defun shell (&rest args)
  (uiop:run-program args :output :string :error-output :string :ignore-error-status t))

(defun tools-present-p ()
  (zerop (nth-value 2 (uiop:run-program '("sh" "-c" "command -v tmux >/dev/null && command -v script >/dev/null && command -v setsid >/dev/null")
                                        :ignore-error-status t))))

(defstruct (tmux-repl (:constructor %make-tmux-repl)) session)

(defun tmux-send (repl text)
  (shell "tmux" "send-keys" "-t" (tmux-repl-session repl) "-l" text)
  (shell "tmux" "send-keys" "-t" (tmux-repl-session repl) "Enter"))

(defun tmux-screen (repl)
  (shell "tmux" "capture-pane" "-p" "-J" "-t" (tmux-repl-session repl) "-S" "-3000"))

(defun tmux-wait (repl marker &key (timeout 120))
  "Дождаться MARKER на экране; экран или NIL."
  (loop repeat (* 10 timeout)
        do (let ((screen (tmux-screen repl)))
             (when (search marker screen) (return screen)))
           (sleep 0.1)))

(defun start-tmux-repl ()
  (let* ((session (format nil "worm-test-~D" (random 1000000)))
         (root (namestring (worm.core:project-root)))
         (command (format nil "cd ~A && LANG=C.UTF-8 sbcl --noinform --no-sysinit --no-userinit --load scripts/init.lisp --eval '(asdf:load-system \"worm\")' --eval '(in-package :worm-user)'"
                          root)))
    (shell "tmux" "-u" "new-session" "-d" "-s" session "-x" "220" "-y" "50" command)
    (%make-tmux-repl :session session)))

(defun stop-tmux-repl (repl)
  (shell "tmux" "kill-session" "-t" (tmux-repl-session repl)))

(defun screen-step (repl tag)
  "Номер шага последнего снимка, напечатанный в REPL с меткой TAG."
  (tmux-send repl (format nil "(format t \"~~&~A=~~D~~%\" (worm.runtime:snapshot-step (snapshot)))" tag))
  (let ((screen (tmux-wait repl (format nil "~A=" tag) :timeout 30)))
    (and screen
         (let* ((start (+ (search (format nil "~A=" tag) screen :from-end t) (length tag) 1))
                (end (position-if-not #'digit-char-p screen :start start)))
           (parse-integer screen :start start :end end)))))

(test repl-in-tmux-handles-a-failure-and-a-dropped-connection
  (if (not (tools-present-p))
      (skip "нет tmux или script")
      (let ((repl (start-tmux-repl))
            (dir (namestring (runtime-dir "repl"))))
        (unwind-protect
             (progn
               (tmux-send repl "(format t \"~&READY~%\")")
               (is (tmux-wait repl "READY" :timeout 180) "REPL поднялся")
               ;; прогон и сбой
               (tmux-send repl (format nil "(progn (start-run 'worm.components:worm-3d :params \"agar-a-v1\" :rings 8 :dt 1d-3 :paused t :directory #p~S) (step-run 50) (format t \"~~&STARTED~~%\"))" dir))
               (is (tmux-wait repl "STARTED" :timeout 120) "прогон запущен")
               (tmux-send repl "(progn (call-in-run (lambda (r) (let ((y (worm.solver:integrator-y-mech (worm.runtime:run-integrator r)))) (loop for k below 8 for i = (* 3 (+ 32 k)) do (setf (aref y (+ i 1)) (* 0.05d0 (aref y (+ i 1))) (aref y (+ i 2)) (* 0.05d0 (aref y (+ i 2)))))))) (resume) (wait-run :status :problem) (format t \"~&FAILED~%\"))")
               (is (tmux-wait repl "FAILED" :timeout 60) "сбой поставил прогон на паузу")
               (tmux-send repl "(problem)")
               (let ((screen (tmux-wait repl "(choose 'rollback-to-checkpoint)" :timeout 30)))
                 (is (not (null screen)) "(problem) показывает варианты")
                 (is (and screen (search "volume-collapse" screen)) "и причину"))
               (tmux-send repl "(progn (choose 'rollback-to-checkpoint) (format t \"~&CHOSEN-~A~%\" (worm.runtime:run-status-of)))")
               (is (tmux-wait repl "CHOSEN-RUNNING" :timeout 30) "(choose …) продолжил счёт")
               ;; клиент tmux в псевдотерминале — как сеанс PuTTY; HUP — обрыв SSH
               (let ((client (uiop:launch-program
                              (list "setsid" "script" "-qfc" (format nil "tmux -u attach -t ~A" (tmux-repl-session repl)) "/dev/null")
                              :input :stream :output nil :error-output nil)))
                 (sleep 1)
                 (is (search (tmux-repl-session repl) (shell "tmux" "list-clients" "-F" "#{session_name}"))
                     "клиент подключён")
                 (shell "pkill" "-HUP" "-f" (format nil "tmux -u attach -t ~A" (tmux-repl-session repl)))
                 (shell "kill" "-HUP" (princ-to-string (uiop:process-info-pid client)))
                 (sleep 1)
                 (is (not (search (tmux-repl-session repl) (shell "tmux" "list-clients" "-F" "#{session_name}")))
                     "клиент отключён")
                 (ignore-errors (uiop:terminate-process client)))
               (is (zerop (nth-value 2 (uiop:run-program (list "tmux" "has-session" "-t" (tmux-repl-session repl))
                                                         :ignore-error-status t)))
                   "сессия жива")
               (let ((a (screen-step repl "STEPA")))
                 (sleep 1)
                 (let ((b (screen-step repl "STEPB")))
                   (is (and a b (> b a)) "счёт идёт после обрыва: шаг ~A → ~A" a b)))
               (tmux-send repl "(progn (stop-run) (format t \"~&STOPPED~%\"))")
               (is (tmux-wait repl "STOPPED" :timeout 60)))
          (stop-tmux-repl repl)))))

;;;; src/io/package.lisp
;;;;
;;;; Слой 4. Файлы прогона: двоичные кадры (один формат для файла и сети),
;;;; frames.bin, meta.json, каталог прогона, контрольные точки на диске.
;;;; Архитектура — «Хранилище и форматы файлов», «Сервер живого просмотра и
;;;; протокол» (формат кадра).

(defpackage #:worm.io
  (:use #:cl #:worm.core)
  (:export
   ;; кадр
   #:+frame-signature+ #:+file-signature+ #:+frame-version+ #:+file-header-size+
   #:frame-size #:encode-frame #:decode-frame
   #:frame #:frame-p #:frame-step #:frame-time #:frame-coords #:frame-activations #:frame-neurons
   ;; frames.bin
   #:frames-writer #:open-frames-writer #:write-frame #:close-frames-writer
   #:frames-writer-count #:frames-writer-path
   #:frames-file #:read-frames-file #:frames-file-count #:frames-file-nodes
   #:frames-file-muscles #:frames-file-neurons #:frames-file-frame-size #:frame-ref
   ;; meta.json и каталог прогона
   #:write-json-file #:read-json-file #:json-object #:json-string #:parse-json
   #:make-run-id #:runs-directory #:make-run-directory #:write-status #:read-status
   #:run-file
   ;; контрольные точки
   #:write-checkpoint-file #:read-checkpoint-file
   ;; условия
   #:format-version-mismatch #:checkpoint-mismatch))

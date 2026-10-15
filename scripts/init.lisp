;;;; scripts/init.lisp — настройка ASDF для проекта Wormlisp.
;;;;
;;;; Загружается первым при любом запуске SBCL (make, scripts/repl.sh,
;;;; рабочие процессы). Делает три вещи:
;;;;   1. подключает ASDF;
;;;;   2. выставляет признак :hunchentoot-no-ssl (сервер без TLS);
;;;;   3. настраивает реестр исходников так, что системы ищутся ТОЛЬКО
;;;;      в vendor/ и в корне репозитория — без Quicklisp и без системных
;;;;      каталогов, чтобы набор библиотек у всех был одинаковым.
;;;;
;;;; Запускать SBCL с --no-userinit: иначе ~/.sbclrc может подключить
;;;; Quicklisp, и его поиск систем обойдёт настройку ниже.

(require :asdf)

(pushnew :hunchentoot-no-ssl *features*)

(let* ((scripts-dir (uiop:pathname-directory-pathname *load-truename*))
       (root (uiop:pathname-parent-directory-pathname scripts-dir))
       (vendor (merge-pathnames "vendor/" root)))
  (asdf:initialize-source-registry
   `(:source-registry
     (:tree ,vendor)
     (:directory ,root)
     :ignore-inherited-configuration)))

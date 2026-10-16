;;;; scripts/make-muscles.lisp — порождает anatomy/muscles.sexp по правилу раскладки.
;;;;
;;;; Запуск из корня репозитория:
;;;;   sbcl --non-interactive --no-userinit --load scripts/init.lisp --load scripts/make-muscles.lisp
;;;;
;;;; Параметры раскладки — docs/parameters.md, «Геометрия тела». Изменив их
;;;; здесь, перезапустите скрипт и закоммитьте файл вместе с документом.

(asdf:load-system "worm/anatomy")

(in-package #:worm.anatomy)

(let* ((layout (make-muscle-layout
                :head-start 0.02d0 :vulva 0.57d0 :tail-end 0.95d0
                :source "WormAtlas (Altun, Hall 2009): ряды и нумерация; вульва 0,57 L по рис. 7A; начало и конец рядов — оценка"))
       (path (merge-pathnames "anatomy/muscles.sexp" (worm.core:project-root))))
  (write-muscles-file
   path (generate-muscles layout) layout
   :comment ";;;; anatomy/muscles.sexp — 95 мышц стенки тела.
;;;;
;;;; Порождён scripts/make-muscles.lisp по правилу раскладки
;;;; (docs/parameters.md, «Геометрия тела»); степень достоверности
;;;; координат — О (оценка). Порядок клеток, ряды (нечётные — медиальные,
;;;; чётные — латеральные) и число клеток в квадрантах — по WormAtlas.
;;;; Положения — доли длины тела: from и to — концы веретена, peak — его
;;;; середина. Правка руками допустима; тест anatomy сообщит, что файл
;;;; разошёлся с правилом.

")
  (format t "~&Записан ~A~%" path))

;;;; src/core/project.lisp — корень репозитория и ревизия git для meta.json.

(in-package #:worm.core)

(defun project-root ()
  "Корень репозитория — каталог, где лежит worm.asd."
  (asdf:system-source-directory "worm/core"))

(defun read-first-line (path)
  (with-open-file (in path :if-does-not-exist nil)
    (and in (string-trim '(#\Space #\Tab #\Return) (or (read-line in nil) "")))))

(defun packed-ref (git-dir ref)
  "Хэш ссылки REF из .git/packed-refs или NIL."
  (with-open-file (in (merge-pathnames "packed-refs" git-dir) :if-does-not-exist nil)
    (when in
      (loop for line = (read-line in nil)
            while line
            do (let ((space (position #\Space line)))
                 (when (and space (string= ref (subseq line (1+ space))))
                   (return (subseq line 0 space))))))))

(defun git-revision ()
  "Полный хэш текущего коммита, прочитанный из .git без запуска git.
Возвращает NIL, если репозитория нет или ссылку прочитать не удалось."
  (let* ((git-dir (merge-pathnames ".git/" (project-root)))
         (head (read-first-line (merge-pathnames "HEAD" git-dir))))
    (cond ((null head) nil)
          ((and (> (length head) 5) (string= "ref: " head :end2 5))
           (let ((ref (subseq head 5)))
             (or (read-first-line (merge-pathnames ref git-dir))
                 (packed-ref git-dir ref))))
          ((= (length head) 40) head)
          (t nil))))

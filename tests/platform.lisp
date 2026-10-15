;;;; tests/platform.lisp — сведения о процессе и проекте.

(in-package #:worm.tests)

(def-suite platform :in worm :description "worm.platform и worm.core.")
(in-suite platform)

(test implementation-is-sbcl
  (is (string= "SBCL" (worm.platform:implementation-description) :end2 4)))

(test memory-and-cores-are-positive
  (is (plusp (worm.platform:dynamic-space-megabytes)))
  (is (plusp (worm.platform:processor-count))))

(test project-root-holds-worm-asd
  (is (probe-file (merge-pathnames "worm.asd" (worm.core:project-root)))))

(test git-revision-is-a-hash-or-nil
  (let ((rev (worm.core:git-revision)))
    (is (or (null rev)
            (and (= 40 (length rev)) (every (lambda (c) (digit-char-p c 16)) rev)))
        "странная ревизия ~S" rev)))

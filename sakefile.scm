(##include "~~prelude/prelude#.scm")
(define lib-directory "lib/")
(define lib-name "playground")
(define lib-suffix ".o1")

(define-task init ()
  (make-directory (current-build-directory)))

(define-task clean (init)
  (delete-file (current-build-directory))
  (delete-file lib-directory))

(define-task compile (init)
  (gambit-eval
    "
    (begin
     (define-cond-expand-feature sdl)
     (include \"~~prelude/prelude#.scm\")
     (compile-file \"module.scm\" output: \"build/playground.o1\"))"))

(define-task install (compile)
  (make-directory lib-directory)
  (delete-file (string-append lib-directory lib-name lib-suffix))
  (copy-file (string-append (current-build-directory) lib-name lib-suffix)
             (string-append lib-directory lib-name lib-suffix)))

(define-task all (compile install)
  '(compile and install))

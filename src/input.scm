;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;; BSD 2-Clause License, for more information see LICENSE

(cond-expand
 (sdl
  (%load playground: sdl/input))
 (android
  (%load playground: android/input)))

(define (input:key-pressed? key-symbol)
  (cond-expand
   (sdl
    ;; TODO: key-name should be transformed into keysym
    (sdl::key-pressed? (sdl::symbol->keysym-sym key-symbol)))))
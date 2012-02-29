;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;; BSD 2-Clause License, for more information see LICENSE

(cond-expand
 (android
  (include "android/resources.scm"))
 (sdl
  (include "sdl/resources.scm"))
 (else))

(define (resources:initialize)
  #t)

(define (resources:config)
  #f)

(define (resources:read-asset)
  #f)

(define (resources:read-file)
  #f)

(define (resources:write-file)
  #f)
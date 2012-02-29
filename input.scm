;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;; BSD 2-Clause License, for more information see LICENSE

(cond-expand
 (android
  (include "android/input.scm"))
 (sdl
  (include "sdl/input.scm"))
 (else))

;(define-structure key-event)

;(define-structure touch-event)

;;
;; Initialization
;;

(define (input:initialize init-config)
  #t)

;;
;; Keyboard
;;

(define (input:key-events)
  #f)

(define (input:key-pressed?)
  #f)

;;
;; Mouse
;;

(define (input:mouse-events)
  #f)

(define (input:mouse-pressed?)
  #f)

(define (input:mouse-released?)
  #f)

(define (input:mouse-x)
  #f)

(define (input:mouse-y)
  #f)

;;
;; Touch
;;

(define (input:touch-events)
  #f)

(define (input:touch-down?)
  #f)

(define (input:touch-x)
  #f)

(define (input:touch-y)
  #f)

;;
;; Accelerometer
;;

(define (input:acceleration-x)
  #f)

(define (input:acceleration-y)
  #f)

(define (input:acceleration-z)
  #f)

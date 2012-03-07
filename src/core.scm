;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;; BSD 2-Clause License, for more information see LICENSE

(cond-expand
 (sdl
  (%load playground: sdl/core))
 (android
  (%load playground: android/core)))

;;; Hooks for C

(define setup-hook (lambda () 0))
(define main-loop-hook (lambda () 5))

;;; Globals

(define *resources* #f)
(define *graphics* #f)
(define *input* #f)
(define *audio* #f)

(define *state* #f)
(define *initialized* #f)

;;; State type: used for passing application state

(define-type state 
  surface
  canvas
  world)

;;; Initialization routine

(define (pg:initialize!)
  (let-syntax ((initialize-module
                (syntax-rules ()
                  ((_ ?global-binding ?init-form)
                   (let ((result ?init-form))
                     (if result
                         (set! ?global-binding result)
                         (error (string-append
                                 "-- Error in module "
                                 (symbol->string '?global-binding)
                                 " -- unable to determine more information"))))))))
    (initialize-module *resources* (resources:initialize))
    (let ((config (resources:config)))
      (initialize-module *graphics* (graphics:initialize config))
      (initialize-module *input* (input:initialize config))
      (initialize-module *audio* (audio:initialize config))))
  0)

;;; Set setup function

(define (pg:set-setup! f)
  (set! setup-hook
        (lambda ()
          (set! *state* (f (pg:initialize!)))
          ;; TODO: check and return error
          0)))

;;; Set draw loop function

(define (pg:set-main-loop! f)
  (set! main-loop-hook
        (lambda ()
          (f *state*)
          ;; TODO: check and return error
          0)))

;;; Start

(define (pg:start!)
  (if *initialized*
      (main-loop-hook)
      (begin
        (setup-hook)
        (set! *initialized* #t)
        (main-loop-hook))))

;;; Window

(define (pg:set-size! state x y
                           ;#!key
                           ;(adaptive #f)
                           ;(fullscreen #f)
                           )
  #f)

(define (pg:width state)
  #f)

(define (pg:height state)
  #f)

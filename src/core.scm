;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;; BSD 2-Clause License, for more information see LICENSE

(cond-expand
 (sdl
  (%include playground: sdl/core))
 (android
  (%include playground: android/core)))

;;; Hooks for C

;(define setup-hook (lambda () 'empty-setup-hook))
(define main-loop-hook (lambda () 'empty-loop-hook))

;;; Globals

(define *resources* 'uninitialized)
(define *graphics* 'uninitialized)
(define *input* 'uninitialized)
(define *audio* 'uninitialized)

;;; Application state

(define-type state
  environment
  world)

;;; Application environment

(define-type environment
  size-x
  size-y
  resources
  graphics)

;;; Initialization routine

(define (pg:initialize! setup-proc)
  (define-macro (initialize-module module init-proc)
    (let ((global-var-name (string->symbol (string-append "*" (symbol->string module) "*"))))
      `(let ((result ,init-proc))
         (if result
             (begin
               (display (string-append
                         "-- initializing Playground: "
                         (symbol->string ',module)
                         "\n"))
               (set! ,global-var-name result)
               result)
             (error (string-append
                     "-- Error in module "
                     ,(symbol->string module)
                     " -- unable to determine more information"))))))
  ;; Resources comes first, as it loads configuration files and global data like assets
  (let* ((resources (initialize-module resources
                                       (resources:initialize)))
         (environment (setup-proc resources)))
    (initialize-module graphics
                       (graphics:initialize
                        (initialize-module audio
                                           (audio:initialize
                                            (initialize-module input
                                                               (input:initialize environment))))))))

(define pg:app
  (let ((state #f))
    (lambda (#!key
        (setup #f)
        (main-loop #f))
      (main-loop (pg:initialize! setup)))))

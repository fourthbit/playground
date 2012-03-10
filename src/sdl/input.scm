;-------------------------------------------------------------------------------
; Initialization
;-------------------------------------------------------------------------------

(define (input:initialize config)
  config)

;-------------------------------------------------------------------------------
; Keyboard
;-------------------------------------------------------------------------------

(define (input:key-events)
  #f)

(define (input:key-pressed? key-symbol)
  (sdl::key-pressed? (sdl::symbol->keysym-sym key-symbol)))

(define (input:call-if-key-pressed proc)
  (if (= (sdl::event-type (sdl::events-get-next)) 2)
      (proc (sdl::event-key-keysym-sym
             (sdl::events-get)))))

;-------------------------------------------------------------------------------
; Mouse
;-------------------------------------------------------------------------------

(define (input:mouse-events)
  #f)

(define input:mouse-pressed? sdl::mouse-pressed?)

(define input:mouse-position sdl::get-mouse-state)

;-------------------------------------------------------------------------------
; Touch
;-------------------------------------------------------------------------------

(define (input:touch-events)
  (error "Touch interface not available in SDL"))

(define (input:touch-down?)
  (error "Touch interface not available in SDL"))

(define (input:touch-x)
  (error "Touch interface not available in SDL"))

(define (input:touch-y)
  (error "Touch interface not available in SDL"))

;-------------------------------------------------------------------------------
; Accelerometer
;-------------------------------------------------------------------------------

(define (input:acceleration-x)
  (error "Accelerator interface not available in SDL"))

(define (input:acceleration-y)
  (error "Accelerator interface not available in SDL"))

(define (input:acceleration-z)
  (error "Accelerator interface not available in SDL"))

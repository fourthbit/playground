;-------------------------------------------------------------------------------
; Initialization
;-------------------------------------------------------------------------------

(define (input:initialize config)
  config)

;-------------------------------------------------------------------------------
; Generic events
;-------------------------------------------------------------------------------

;;; Poll all events in the event queue and call proc each time

(define (input:call-with-poll-events proc)
  (let loop ()
    (receive (more? event)
             (sdl::poll-event)
             (if more?
                 (begin (proc event) (loop))
                 (proc event)))))

;;; Wait for event and then call proc

(define (input:call-with-wait-event proc)
  (proc (sdl::wait-event)))

;-------------------------------------------------------------------------------
; Keyboard
;-------------------------------------------------------------------------------

(define (input:event-key? event)
  (= sdl::key-down (sdl::event-type event)))

(define (input:event->key event)
  (sdl::keysym-sym->symbol
   (sdl::event-key-keysym-sym event)))

(define (input:key-pressed? key-symbol)
  (sdl::key-pressed? (sdl::symbol->keysym-sym key-symbol)))

(define-macro (define-key key)
  `(define ,(macro-append 'input: key)
     ,(macro-append 'sdl:: key)))

;; TODO
; (define-key key-backspace)
;(pp input:key-backspace)

;-------------------------------------------------------------------------------
; Mouse
;-------------------------------------------------------------------------------

(define (input:event-mouse-pressed? event)
  (= sdl::mouse-button-down (sdl::event-type event)))

(define (input:event->mouse event)
  #f)

;;; Check if mouse if mouse button is pressed

(define (input:mouse-button-pressed? button)
  (sdl::mouse-pressed? (sdl::mouse-button->button-num button)))

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

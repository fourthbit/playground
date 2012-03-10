;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;; BSD 2-Clause License, for more information see LICENSE

(cond-expand
 (sdl
  (%load playground: sdl/graphics))
 (android
  (%load playground: android/graphics)))

;;; How to deal with non-floating-point input to cairo functions

(define-macro (flonum arg)
  `(exact->inexact ,arg))

;-------------------------------------------------------------------------------
; System
;-------------------------------------------------------------------------------

(define-type graphics
  surface
  canvas)

(define *cairo* #f)

(define (graphics:set-cairo! cairo)
  (set! *cairo* cairo))

(define (graphics:initialize env)
  (cond-expand
   (sdl
    (environment-graphics-set! env
                               (pg:sdl-initialize (environment-size-x env)
                                                  (environment-size-y env)))
    env)
   (android
    #f)))

(define draw:on
  (cond-expand
   (android
    #f)
   (sdl
    sdl::flip)))

;-------------------------------------------------------------------------------
; Color
;-------------------------------------------------------------------------------

(define-record-type color
  make-color%internal
  color?
  r
  g
  b
  a)

(define (make-color/rgb r g b)
  (make-color%internal (flonum r)
                       (flonum g)
                       (flonum b)
                       1.0))

(define (make-color/rgba r g b a)
  (make-color%internal (flonum r)
                       (flonum g)
                       (flonum b)
                       (flonum a)))

;-------------------------------------------------------------------------------
; Pixmap
;-------------------------------------------------------------------------------

;(define-structure pixmap)

(define (draw:pixmap-load filename format)
  #f)

(define (draw:pixmap-width pixmap)
  #f)

(define (draw:pixmap-height pixmap)
  #f)

(define (draw:pixmap-format pixmap)
  #f)

(define (draw:pixmap-dispose pixmap)
  #f)

(define (draw:pixmap pixmap x y src-x src-y src-width src-height)
  #f)

;-------------------------------------------------------------------------------
; Stroke and Fill
;-------------------------------------------------------------------------------

;;; Stroke

(define-type pattern
  sequence)

(define-type stroke
  color
  thickness
  pattern)

(define *default-stroke*
  (make-stroke #f
               #f
               #f))

(define (draw:stroke! color thickness pattern
                 #!key
                 (cairo *cairo*))
  (set! *default-stroke* (make-stroke color
                                      thickness
                                      pattern)))

(define (draw:stroke-color! color
                       #!key
                       (cairo *cairo*))
  (stroke-color-set! *default-stroke* color))

(define (draw:stroke-thickness! thickness
                           #!key
                           (cairo *cairo*))
  (stroke-thickness-set! *default-stroke* thickness))

;;; Fill

(define-type pattern2d
  sequence)

(define-type fill
  color
  pattern2d)

(define *default-fill*
  (make-fill #f
             #f))

(define (draw:fill! color pattern2d
               #!key
               (cairo *cairo*))
  (set! *default-fill* (make-fill color pattern2d)))

(define (draw:fill-color! c
                     #!key
                     (cairo *cairo*))
  (fill-color-set! *default-fill* c))

;;; Execute paint: stroke, fill or both

(define (%%execute-paint cairo stroke fill)
  (let ((preserve? stroke))
    (if fill
        (if (fill-color fill)
            (let ((c (fill-color fill)))
              (cairo:set-source-rgba cairo
                                     (color-r c)
                                     (color-g c)
                                     (color-b c)
                                     (color-a c))
              ((if preserve? cairo:fill-preserve cairo-fill)
               cairo))))
    (if stroke
        (if (stroke-color stroke)
            (let ((c (stroke-color stroke)))
              (cairo:set-source-rgba cairo
                                     (color-r c)
                                     (color-g c)
                                     (color-b c)
                                     (color-a c))
              (cairo:set-line-width cairo (stroke-thickness stroke))
              (cairo:stroke cairo))))))

;-------------------------------------------------------------------------------
; Pixels
;-------------------------------------------------------------------------------

(define (draw:clear-pixels color)
  (error "Not implemented"))

(define (draw:get-pixel x y color)
  (error "Not implemented"))

;-------------------------------------------------------------------------------
; Shapes
;-------------------------------------------------------------------------------

(define (draw:segment x1 y1 x2 y2
                 #!key
                 (cairo *cairo*)
                 (stroke *default-stroke*)
                 (fill *default-fill*))
  (define cairo *cairo*)
  (define stroke *default-stroke*)
  (define fill *default-fill*)
  (cairo:move-to cairo (flonum x1) (flonum y1))
  (cairo:line-to cairo (flonum x2) (flonum y2))
  (%%execute-paint cairo stroke fill))

;;; Draw a rectangle given its two corners

(define (draw:rectangle/corners x1 y1 x2 y2
                           #!key
                           (cairo *cairo*)
                           (stroke *default-stroke*)
                           (fill *default-fill*))
  (define cairo *cairo*)
  (define stroke *default-stroke*)
  (define fill *default-fill*)
  (cairo:rectangle cairo
                   (flonum x1) (flonum y1)
                   (flonum x2) (flonum y2))
  (%%execute-paint cairo stroke fill))

;;; Draw a rectangle given its center and its sides

(define (draw:rectangle/center)
  (error "Not implemented"))

;;; Draw a rectangle given the top-left corner and its sides

(define (draw:rectangle/corner-sides x y width height
                                #!key
                                (cairo *cairo*)
                                (stroke *default-stroke*)
                                (fill *default-fill*))
  (error "Not implemented"))

;;; Draw an ellipse given its two corners

(define (draw:ellipse/corners x1 y1 x2 y2)
  (error "Not implemented"))

;; Draw an ellipse given the top-left corner and its sides

(define (draw:ellipse/corner-sides x y width height)
  (error "Not implemented"))

;;; Draw an ellipse given a center and its width and height

(define (draw:ellipse/center x y width height)
  (error "Not implemented"))

;;; Draw circle from given a center and its radius

(define (draw:circle/center x y r
                            #!key
                            (cairo *cairo*)
                            (stroke *default-stroke*)
                            (fill *default-fill*))
  (define cairo *cairo*)
  (define stroke *default-stroke*)
  (define fill *default-fill*)
  (cairo:arc cairo
             (flonum x)
             (flonum y)
             (flonum r)
             0.0
             ;TODO
             ;pi2
             6.28
             )
  (%%execute-paint cairo stroke fill))

;;; Bezier curve

(define (draw:bezier pl
                     #!key
                     (cairo *cairo*)
                     (stroke #f)
                     (fill #f))
  (error "Not implemented"))
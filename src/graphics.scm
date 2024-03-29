;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;; BSD 2-Clause License, for more information see LICENSE

(cond-expand
 (sdl
  (%include playground: sdl/graphics))
 (android
  (%include playground: android/graphics)))

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
    (let ((sdl:initialize
           (lambda (size-x size-y)
             (let* (    ;(osx-only (%if-sys "Darwin" (SDL::init-osx)))
                    (error (sdl::init sdl::init-video)) ; TODO: check this error
                    (sdl-surface (sdl::set-video-mode size-x size-y 0 (+ sdl::hwsurface
                                                                         sdl::hwpalette
                                                                         sdl::doublebuf)))
                    (image-surface (cairo:image-surface-create-for-data
                                    (sdl::surface-pixels sdl-surface)
                                    cairo:format-argb32
                                    size-x
                                    size-y
                                    (sdl::screen-pitch sdl-surface)))
                    (cairo (cairo:create image-surface)))
               (graphics:set-cairo! cairo) ; TODO: should be part of module initialization
               (make-graphics sdl-surface cairo)))))
      (environment-graphics-set! env
                                 (sdl:initialize (environment-size-x env)
                                                 (environment-size-y env))))
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

(define-type color
  constructor: make-color%internal
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

(define *default-stroke* #f)

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

(define (draw:no-stroke! #!key
                         (cairo *cairo*))
  (set! *default-stroke* #f))

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

(define (draw:fill! color pattern2d)
  (set! *default-fill* (make-fill color pattern2d)))

(define (draw:fill-color! c)
  (fill-color-set! *default-fill* c))

(define (draw:no-fill! #!key
                       (cairo *cairo*))
  (fill-color-set! *default-fill* #f))

;;; Execute paint: stroke, fill or both

(define (%%execute-paint cairo stroke fill)
  (if fill
      (if (fill-color fill)
          (let ((c (fill-color fill)))
            (cairo:set-source-rgba cairo
                                   (color-r c)
                                   (color-g c)
                                   (color-b c)
                                   (color-a c))
            ((if stroke cairo:fill-preserve cairo:fill)
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
            (cairo:stroke cairo)))))

;-------------------------------------------------------------------------------
; Pixels
;-------------------------------------------------------------------------------

(define (draw:clear-pixels color)
  (error "Not implemented"))

(define (draw:get-pixel x y color)
  (error "Not implemented"))

;-------------------------------------------------------------------------------
; Operations
;-------------------------------------------------------------------------------

(define (draw:translate x y
                        #!key
                        (cairo *cairo*))
  (cairo:translate cairo (flonum x) (flonum y)))

(define (draw:scale x y
                    #!key
                    (cairo *cairo*))
  (cairo:scale cairo (flonum x) (flonum y)))

(define (draw:rotate angle
                    #!key
                    (cairo *cairo*))
  (cairo:rotate cairo angle))

(define (draw:restore #!key (cairo *cairo*))
  (cairo:restore cairo))

;-------------------------------------------------------------------------------
; Shapes
;-------------------------------------------------------------------------------

(define (draw:segment x1 y1 x2 y2
                 #!key
                 (cairo *cairo*)
                 (stroke *default-stroke*)
                 (fill *default-fill*))
  (cairo:move-to cairo (flonum x1) (flonum y1))
  (cairo:line-to cairo (flonum x2) (flonum y2))
  (%%execute-paint cairo stroke fill))

(define (draw:arc x y radius angle1 angle2
                  #!key
                  (cairo *cairo*)
                  (stroke *default-stroke*)
                  (fill *default-fill*))
  (cairo:arc cairo
             (flonum x) (flonum y)
             (flonum radius)
             (flonum angle1) (flonum angle2))
  (%%execute-paint cairo stroke fill))

(define (draw:arc-negative x y radius angle1 angle2
                  #!key
                  (cairo *cairo*)
                  (stroke *default-stroke*)
                  (fill *default-fill*))
  (cairo:arc-negative cairo
                      (flonum x) (flonum y)
                      (flonum radius)
                      (flonum angle1) (flonum angle2))
  (%%execute-paint cairo stroke fill))

;;; Draw a rectangle given its two corners

(define (draw:rectangle/corner-corner x1 y1 x2 y2
                                      #!key
                                      (cairo *cairo*)
                                      (stroke *default-stroke*)
                                      (fill *default-fill*))
  (let ((x1 (flonum x1))
        (y1 (flonum y1)))
    (cairo:rectangle cairo
                     x1 y1
                     (fl- (flonum x2) x1) (fl- (flonum y2) y1)))
  (%%execute-paint cairo stroke fill))

;;; Draw a rectangle given its center and its sides

(define (draw:rectangle/center-sides x y width height
                                     #!key
                                     (cairo *cairo*)
                                     (stroke *default-stroke*)
                                     (fill *default-fill*))
  (let ((x (flonum x))
        (y (flonum y))
        (width (flonum width))
        (height (flonum height)))
    (let ((half-width (fl/ width 2.0))
          (half-height (fl/ height 2.0)))
      (cairo:rectangle cairo
                       (fl- x half-width) (fl- y half-height)
                       width height)))
  (%%execute-paint cairo stroke fill))

;;; Draw a rectangle given the top-left corner and its sides

(define (draw:rectangle/corner-sides x y width height
                                     #!key
                                     (cairo *cairo*)
                                     (stroke *default-stroke*)
                                     (fill *default-fill*))
  (cairo:rectangle cairo
                   (flonum x) (flonum y)
                   (flonum width) (flonum height))
  (%%execute-paint cairo stroke fill))

;;; Draw an ellipse given a center and its width and height

(define (draw:ellipse/center x y width height
                             #!key
                             (cairo *cairo*)
                             (stroke *default-stroke*)
                             (fill *default-fill*))
  (cairo:save cairo)
  (cairo:translate cairo (flonum x) (flonum y))
  (cairo:scale cairo
               (fl/ (flonum width) 2.0)
               (fl/ (flonum height) 2.0))
  (cairo:arc cairo
             0.0 0.0
             1.0
             0.0 pi2)
  (cairo:restore cairo)
  (%%execute-paint cairo stroke fill))


;;; Draw an ellipse given its two corners

(define (draw:ellipse/corner-corner x1 y1 x2 y2)
  (let ((x1 (flonum x1))
        (y1 (flonum y1))
        (x2 (flonum x2))
        (y2 (flonum y2)))
    (let ((width (fl- x2 x1))
          (height (fl- y2 y1)))
     (draw:ellipse/center (+ x1 (fl/ width 2.0))
                          (+ y1 (fl/ height 2.0))
                          width
                          height))))

;; Draw an ellipse given the top-left corner and its sides

(define (draw:ellipse/corner-sides x y width height
                                   #!key
                                   (cairo *cairo*)
                                   (stroke *default-stroke*)
                                   (fill *default-fill*))
  (let ((x (flonum x))
        (y (flonum y))
        (width (flonum width))
        (height (flonum height)))
   (draw:ellipse/center (+ x (fl/ width 2.0))
                        (+ y (fl/ height 2.0))
                        width
                        height)))


;;; Draw circle from given a center and its radius

(define (draw:circle/center x y r
                            #!key
                            (cairo *cairo*)
                            (stroke *default-stroke*)
                            (fill *default-fill*))
  (cairo:arc cairo
             (flonum x) (flonum y)
             (flonum r)
             0.0 pi2)
  (%%execute-paint cairo stroke fill))

;;; Draw polygon

(define (draw:path #!key
                   (closed? #f)
                   (cairo *cairo*)
                   (stroke *default-stroke*)
                   (fill *default-fill*)
                   #!rest points)
  (let ((first (car points)))
    (cairo:move-to cairo
                   (flonum (vect2-x first))
                   (flonum (vect2-y first))))
  (let recur ((points (cdr points)))
    (unless (null? points)
            (cairo:line-to cairo
                           (flonum (vect2-x (car points)))
                           (flonum (vect2-y (car points))))
            (recur (cdr points))))
  (if closed?
      (cairo:close-path cairo))
  (%%execute-paint cairo stroke fill))

;;; Bezier curve

(define (draw:bezier p1 p2 p3 p4
                     #!key
                     (cairo *cairo*)
                     (stroke *default-stroke*)
                     (fill *default-fill*))
  (cairo:move-to cairo (flonum (vect2-x p1)) (flonum (vect2-y p1)))
  (cairo:curve-to cairo
                  (flonum (vect2-x p2)) (flonum (vect2-y p2))
                  (flonum (vect2-x p3)) (flonum (vect2-y p3))
                  (flonum (vect2-x p4)) (flonum (vect2-y p4)))
  (%%execute-paint cairo stroke fill))

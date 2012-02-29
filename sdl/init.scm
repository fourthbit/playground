;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.

(import (ffi-sdl))
(import (ffi-cairo))
(import (base system-conditional))

(import (anygame anygame
                 graphics))

(define (sdl:initialize size-x size-y)
  (let* ((osx-only (%if-sys "Darwin" (SDL::init-osx)))
         (error (SDL::init SDL::init-video)) ; TODO: check this error
         (sdl-surface (SDL::set-video-mode size-x size-y 0 (+ SDL::hwsurface
                                                              SDL::hwpalette
                                                              SDL::doublebuf)))
         (image-surface (cairo-image-surface-create-for-data
                         (SDL::surface-pixels sdl-surface)
                         CAIRO_FORMAT_ARGB32
                         size-x
                         size-y
                         (SDL::screen-pitch sdl-surface)))
         (cairo (cairo-create image-surface)))
    (graphics:set-cairo! cairo) ; TODO: should be part of module initialization
    (make-state sdl-surface cairo '())))
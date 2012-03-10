;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.

(define (pg:sdl-initialize size-x size-y)
  (let* (;(osx-only (%if-sys "Darwin" (SDL::init-osx)))
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
    (make-graphics sdl-surface cairo)))

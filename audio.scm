;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;; BSD 2-Clause License, for more information see LICENSE

(cond-expand
 (android
  (include "android/audio.scm"))
 (sdl
  (include "sdl/audio.scm"))
 (else))

(define (audio:initialize init-config)
  #t)

(define (audio:set-volume!)
  #f)

(define (audio:music-play)
  #f)

(define (audio:music-stop)
  #f)

(define (audio:music-pause)
  #f)

(define (audio:music-playing?)
  #f)

(define (audio:music-stopped?)
  #f)

(define (audio:music-looping?)
  #f)

(define (audio:music-dispose)
  #f)

(define (audio:sound-play)
  #f)

(define (audio:sound-dispose)
  #f)

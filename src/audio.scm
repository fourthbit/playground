;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;; BSD 2-Clause License, for more information see LICENSE

(cond-expand
 (sdl
  (%load playground: sdl/audio))
 (android
  (%load playground: android/audio)))


(define (audio:initialize config)
  config)

(define (audio:set-volume!)
  #f)

(define-structure music
  file
  id
  volume
  playing?
  loop?)

(define (audio:music-load filename)
  #f)

(define (audio:music-play music)
  #f)

(define (audio:music-stop music)
  #f)

(define (audio:music-pause music)
  #f)

(define audio:music-playing? music-playing?)

(define audio:music-loop? music-loop?)

(define (audio:music-dispose music)
  #f)

(define-structure sound
  file
  id
  volume)

(define (audio:sound-play)
  #f)

(define (audio:sound-dispose)
  #f)

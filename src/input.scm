;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;; BSD 2-Clause License, for more information see LICENSE

(cond-expand
 (sdl
  (%load playground: sdl/input))
 (android
  (%load playground: android/input)))

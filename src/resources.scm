;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;; BSD 2-Clause License, for more information see LICENSE

(cond-expand
 (sdl
  (%include playground: sdl/resources))
 (android
  (%include playgorund: android/resources)))

(define (resources:initialize)
  'no-resources-loaded)

;;; Read a file as a u8vector

(define (resources:read-file filename)
  (call-with-input-file
      filename
    (lambda (port)
      (let* ((fs (file-size filename))
             (u8v (make-u8vector fs)))
        (read-subu8vector u8v 0 fs port)
        u8v))))

;;; Write a u8vector to a file

(define (resources:write-file filename u8v-data)
  (call-with-output-file
      filename
    (lambda (port)
      (write-subu8vector u8v-data 0 (u8vector-length u8v-data) port))))
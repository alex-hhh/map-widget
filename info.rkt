#lang info
(define collection "map-widget")
(define deps '("draw-lib"
               "errortrace-lib"
               "gui-lib"
               "db-lib"
               "math-lib"
               "base"
               "http-easy"
               "geoid"
               "rackunit-lib"))
(define build-deps '("scribble-lib"
                     "draw-doc"
                     "gui-doc"
                     "racket-doc"
                     "al2-test-runner"))
(define scribblings '(("scribblings/map-widget.scrbl" ())))
(define pkg-desc "Map Widget based on Open Street Map tiles")
(define version "0.0")
(define pkg-authors '(aharsanyi))

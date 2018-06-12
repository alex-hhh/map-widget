#lang racket/base

(module+ test
  (require rackunit))

(require "private/map-widget.rkt"
         "private/utilities.rkt"
         "private/map-util.rkt"
         "private/map-tiles.rkt")

(provide map-widget%
         map-widget-logger
         map-distance/radians
         map-distance/degrees
         map-bearing/radians
         map-bearing/degrees
         get-tile-provider-names
         current-tile-provider-name
         set-current-tile-provider)

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; To rebuild package
;;
;;   $ raco setup --no-pkg-deps map-widget
;;   $ raco setup --doc-index --no-pkg-deps map-widget
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(module+ test
  ;; Tests to be run with raco test
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )

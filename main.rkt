;; SPDX-License-Identifier: LGPL-3.0-or-later
;; main.rkt -- main file for the map-widget package, exports the widgets
;;
;; This file is part of map-widget -- A Racket GUI Widget to display maps based on OpenStreetMap tiles
;; Copyright (c) 2020, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;; License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

#lang racket/base

(require "private/map-widget.rkt"
         "private/map-snip.rkt"
         "private/utilities.rkt"
         "private/map-util.rkt"
         "private/tiles.rkt"
         "private/layers.rkt")

(provide map-widget%
         map-snip%
         map-widget-logger
         min-zoom-level
         max-zoom-level
         get-tile-providers
         current-tile-provider
         set-current-tile-provider
         allow-tile-download
         set-allow-tile-download
         vacuum-tile-cache-database
         shutdown-map-tile-workers

         layer<%>
         lines-layer%
         markers-layer%
         points-layer%
         point-cloud-layer%
         current-location-layer%

         lines-layer
         line-layer
         points-layer
         markers-layer
         point-cloud-layer
         current-location-layer)

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

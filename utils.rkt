;; utils.rkt -- map utilities
;;
;; This file is part of map-widget -- A Racket GUI Widget to display maps based on OpenStreetMap tiles
;; Copyright (c) 2020 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require "private/map-util.rkt")

(provide map-distance/radians
         map-distance/degrees
         map-bearing/radians
         map-bearing/degrees

         (struct-out npoint)
         lat-lon->npoint
         npoint->lat-lon

         (struct-out bbox)
         track-bbox
         bbox-center
         bbox-center/ndcs
         bbox-size
         bbox-merge
         bbox-extend)


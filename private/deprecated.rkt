#lang racket/base
;; SPDX-License-Identifier: LGPL-3.0-or-later
;; deprecated.rkt -- deprecated interface implementation
;;
;; This file is part of map-widget -- A Racket GUI Widget to display maps
;; based on OpenStreetMap tiles
;;
;; Copyright (c) 2019, 2024, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require "layers.rkt"
         racket/class)
(provide (all-defined-out))

;; The functions below implement the original map-widget% API in terms of
;; layers.  They are here, so map-impl% class does not have to have deprecated
;; functions.  map-snip% and map-widget% still provide the old methods, and
;; call into these functions for implementation.

(define (find-or-create-layer map-impl type name)
  (define layer (send map-impl find-layer-by-name name))
  (when (and layer (not (is-a? layer type)))
    (error "find-or-create-layer: layer ~a exists but it is not a ~a" name type))
  (if layer
      layer
      (let ([new-layer (new type [name name])])
        (send map-impl add-layer new-layer)
        new-layer)))

(define (deprecated-add-track map-impl track (group 'deprecated-lines-layer))
  (define layer (find-or-create-layer map-impl lines-layer% group))
  (send layer add-line track))

(define (deprecated-add-marker map-impl pos text direction color)
  (define layer (find-or-create-layer map-impl markers-layer% 'deprecated-markers-layer ))
  (send layer add-marker pos text direction color))

(define (deprecated-set-point-cloud-colors map-impl cm)
  (define pcl (find-or-create-layer map-impl point-cloud-layer% 'deprecated-point-cloud-layer))
  (send pcl set-color-map cm))

(define (deprecated-add-to-point-cloud map-impl points #:format (fmt 'lat-lng))
  (define pcl (find-or-create-layer map-impl point-cloud-layer% 'deprecated-point-cloud-layer))
  (send pcl add-points points #:format fmt))

(define (deprecated-get-point-count map-impl)
  (define pcl (send map-impl find-layer-by-name 'deprecated-point-cloud-layer))
  (if pcl
      (send pcl get-point-count)
      (values 0 0)))

(define (deprecated-clear-point-cloud map-impl)
  (define pcl (send map-impl find-layer-by-name 'deprecated-point-cloud-layer))
  (when pcl
    (send pcl clear)
    (send map-impl refresh)))

(define deprecated-current-location
  (case-lambda
    ((map-impl)
     (let ([cll (find-or-create-layer map-impl current-location-layer% 'deprecated-current-location-layer)])
       (send cll current-location)))
    ((map-impl pos)
     (let ([cll (find-or-create-layer map-impl current-location-layer% 'deprecated-current-location-layer)])
       (send cll current-location pos)))))

(define deprecated-track-current-location
  (case-lambda
    ((map-impl)
     (let ([cll (find-or-create-layer map-impl current-location-layer% 'deprecated-current-location-layer)])
       (send cll track-current-location)))
    ((map-impl flag)
     (let ([cll (find-or-create-layer map-impl current-location-layer% 'deprecated-current-location-layer)])
       (send cll track-current-location flag)))))

(define (deprecated-set-group-pen map-impl group pen)
  (if group
      (let ([l (find-or-create-layer map-impl lines-layer% group)])
        (send l set-pen pen))
      (dynamic-wind
        (lambda ()
          (send map-impl begin-edit-sequence))
        (lambda ()
          (find-or-create-layer map-impl lines-layer% 'deprecated-lines-layer)
          (for ([l (in-list (send map-impl get-all-layers))]
                #:when (is-a? l lines-layer%))
            (send l set-pen pen)))
        (lambda ()
          (send map-impl end-edit-sequence)))))

(define (deprecated-set-group-zorder map-impl group zorder)
  (if group
      (let ([l (find-or-create-layer map-impl lines-layer% group)])
        (send l set-zorder zorder))
      (dynamic-wind
        (lambda ()
          (send map-impl begin-edit-sequence))
        (lambda ()
          (find-or-create-layer map-impl lines-layer% 'deprecated-lines-layer)
          (for ([l (in-list (send map-impl get-all-layers))]
                #:when (is-a? l lines-layer%))
            (send l set-zorder zorder)))
        (lambda ()
          (send map-impl end-edit-sequence)))))

(define (deprecated-clear map-impl)
  (for ([l (in-list (send map-impl get-all-layers))])
    (when (or (is-a? l lines-layer%)
              (is-a? l markers-layer%)
              (is-a? l point-cloud-layer%))
      (send l clear)))
  (send map-impl refresh))

(define (deprecated-delete-group map-impl group)
  (if group
      (let ([l (send map-impl find-layer-by-name group)])
        (when l
          (when (or (is-a? l lines-layer%)
                    (is-a? l markers-layer%)
                    (is-a? l point-cloud-layer%))
            (send l clear))))
      (for ([l (in-list (send map-impl get-all-layers))])
        (when (or (is-a? l lines-layer%)
                  (is-a? l markers-layer%)
                  (is-a? l point-cloud-layer%))
          (send l clear)))))

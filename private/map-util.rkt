#lang racket/base
;; map-util.rkt -- various utilities related to maps

;; This file is part of map-widget
;; Copyright (c) 2018 Alex Harsanyi <AlexHarsanyi@gmail.com>

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;; License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require math/base
         racket/flonum
         racket/math
         racket/contract
         racket/sequence
         racket/stream
         "utilities.rkt")

;; NOTE: provides are at the end of the file


;;.................................... distance and bearing calculations ....

;; Formulas from http://www.movable-type.co.uk/scripts/latlong.html

(define earth-radius (->fl 6371000))    ; meters

(define (haversin theta)
  (fl/ (fl- 1.0 (flcos theta)) 2.0))

(define (inv-haversin h)
  (fl* 2.0 (flasin (flsqrt h))))

;; Calculate the distance in meters between two map coordinates
(define (map-distance/radians lat1 lon1 lat2 lon2)
  (let ((delta-lat (fl- lat2 lat1))
        (delta-lon (fl- lon2 lon1)))
    (let* ((a (fl+ (haversin delta-lat)
                   (fl* (fl* (flcos lat1) (flcos lat2))
                        (haversin delta-lon))))
           (c (inv-haversin a)))
      (fl* c earth-radius))))

(define (map-distance/degrees lat1 lon1 lat2 lon2)
  (map-distance/radians
   (degrees->radians lat1)
   (degrees->radians lon1)
   (degrees->radians lat2)
   (degrees->radians lon2)))

;; Calculate the initial bearing for traveling between two map coordinates
;; (bearing is returned in radians).  note that the bearing will have to
;; change as one travers towards lat2, lon2 and has to be re-computed
;; periodically.
(define (map-bearing/radians lat1 lon1 lat2 lon2)
  (let ((delta-lon (fl- lon2 lon1)))
    (let ((y (fl* (flsin delta-lon) (flcos lat2)))
          (x (fl- (fl* (flcos lat1) (flsin lat2))
                  (fl* (fl* (flsin lat1) (flcos lat2)) (flcos delta-lon)))))
      (flatan (/ y x)))))

(define (map-bearing/degrees lat1 lon1 lat2 lon2)
  (map-bearing/radians
   (degrees->radians lat1)
   (degrees->radians lon1)
   (degrees->radians lat2)
   (degrees->radians lon2)))


;;................................................. normalized map point ....

;; Normalized map coordinate point. (0,0) is the top-left corner of the entire
;; world map and (1,1) is the bottom right.
(struct npoint (x y)
  #:transparent
  #:guard
  (lambda (x y name)
    (if (and (>= x 0.0) (<= x 1.0)
             (>= y 0.0) (<= y 1.0))
        (values x y)
        (let ((msg (format "~a - bad coordinates: ~a ~a" name x y)))
          (log-message map-widget-logger 'error #f msg)
          (error msg)))))

(define (lat-lon->npoint lat lon)
  (let ((r-lat (degrees->radians lat))
        (r-lon (degrees->radians lon)))
    (let ((x r-lon)
          (y (asinh (tan r-lat))))
      (let ((x-norm (/ (+ 1 (/ x pi)) 2))
            (y-norm (/ (- 1 (/ y pi)) 2)))
        (npoint x-norm y-norm)))))

(define (npoint->lat-lon p)
  (let ((x-norm (npoint-x p))
        (y-norm (npoint-y p)))
    (let ((x (* (- (* x-norm 2) 1) pi))
          (y (* (- 1 (* y-norm 2)) pi)))
      (values
       (radians->degrees (atan (sinh y)))
       (radians->degrees x)))))


;;......................................................... bounding box ....

;; Bounding box defines a rectangular region on the map.
(struct bbox (max-lat max-lon min-lat min-lon) #:transparent)

(define (point-lat p) (vector-ref p 0))
(define (point-lon p) (vector-ref p 1))

;; Return the bounding box of a TRACK (expressed as a sequence of GPS
;; coordinates, latitude and longitude degrees.  Each element in TRACK should
;; be a vector of at least two elements: the first one is the latitude, the
;; second one, the longitude.
(define (track-bbox track)
  (let ((min-lat 180.0)
        (min-lon 180.0)
        (max-lat -180.0)
        (max-lon -180.0))
    (for ([point track])
      (let ((lat (point-lat point))
            (lon (point-lon point)))
        (set! min-lat (min lat min-lat))
        (set! max-lat (max lat max-lat))
        (set! min-lon (min lon min-lon))
        (set! max-lon (max lon max-lon))))
    (bbox max-lat max-lon min-lat min-lon)))

;; Return the center of the bounding box BBOX
(define (bbox-center bbox)
  (let ((lat (/ (+ (bbox-max-lat bbox) (bbox-min-lat bbox)) 2))
        (lon (/ (+ (bbox-max-lon bbox) (bbox-min-lon bbox)) 2)))
    (values lat lon)))

;; Return the center of the bounding box as a normalized coordinate point.
(define (bbox-center/ndcs bbox)
  (let ((map1 (lat-lon->npoint (bbox-max-lat bbox) (bbox-max-lon bbox)))
        (map2 (lat-lon->npoint (bbox-min-lat bbox) (bbox-min-lon bbox))))
    (let ((cx (/ (+ (npoint-x map1) (npoint-x map2)) 2))
          (cy (/ (+ (npoint-y map1) (npoint-y map2)) 2)))
      (npoint cx cy))))

;; Return the dimensions (width and height) of the BBOX in meters.
(define (bbox-size bbox)
  (let ((min-lat (bbox-min-lat bbox))
        (min-lon (bbox-min-lon bbox))
        (max-lat (bbox-max-lat bbox))
        (max-lon (bbox-max-lon bbox)))
    (values (map-distance/degrees min-lat min-lon min-lat max-lon)
            (map-distance/degrees min-lat min-lon max-lat min-lon))))

;; Join two bounding boxes, returning a bounding box that encloses both of
;; them.
(define (bbox-merge bb1 bb2)
  (bbox
   (max (bbox-max-lat bb1) (bbox-max-lat bb2))
   (max (bbox-max-lon bb1) (bbox-max-lon bb2))
   (min (bbox-min-lat bb1) (bbox-min-lat bb2))
   (min (bbox-min-lon bb1) (bbox-min-lon bb2))))

(define (bbox-extend bb pos)
  (bbox-merge bb (bbox (point-lat pos) (point-lon pos) (point-lat pos) (point-lon pos))))

;; Maximum zoom level we allow for the map widget.
(define max-zl
  (get-pref 'map-widget:max-map-zoom-level (lambda () 16)))
(define min-zl 1)

(define (max-zoom-level) max-zl)
(define (min-zoom-level) min-zl)

;; Tiles are provided at zoom levels between 1 and 18, note that this is
;; different from `get-max-zoom-level`, which is an application option.
(define (valid-zoom-level? z) (and (>= z 1) (<= z 18)))

;; A map is drawn as a set of tiles, each tile is a 256x256 pixel image.  See
;; also: http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
(struct tile (zoom x y)
  #:transparent
  #:guard
  (lambda (zoom x y name)
    (unless (valid-zoom-level? zoom)
      (define msg (format "invalid zoom level (~a): ~a" name zoom))
      (log-message map-widget-logger 'error #f msg)
      (error msg))
    (let ((max-val (expt 2 zoom)))
      (unless (and (>= x 0) (< x max-val))
        (define msg (format "~a - bad x: ~a (valid range 0..~a)" name x max-val))
        (log-message map-widget-logger 'error #f msg)
        (error msg))
      (unless (and (>= y 0) (< y max-val))
        (define msg (format "~a - bad y: ~a (valid range 0..~a)" name y max-val))
        (log-message map-widget-logger 'error #f msg)
        (error msg)))
    (values zoom x y)))

(define (tile-equal? tile1 tile2)
  (and (equal? (tile-zoom tile1) (tile-zoom tile2))
       (equal? (tile-x tile1) (tile-x tile2))
       (equal? (tile-y tile1) (tile-y tile2))))


;; convert a zoom level to a "meters per pixel" value
(define (zoom-level->mpp zoom-level)
  (let ((n (expt 2 (+ 8 zoom-level))))
    (/ (* 2 pi earth-radius) n)))

;; convert a "meters per pixel" value to an approximate zoom level
(define (mpp->zoom-level mpp)
  (let ((n (/ (* 2 pi earth-radius) mpp)))
    (exact-truncate (- (/ (log n) (log 2)) 8))))

;; Return a zoom-level such that the contents of BBOX will fit in a canvas of
;; CANVAS-WIDTH, CANVAS-HEIGHT pixels
(define (select-zoom-level bbox canvas-width canvas-height)
  (unless bbox (error "select-zoom-level" bbox))
  (let-values (([w h] (bbox-size bbox)))
    ;; If the BBOX is too small, just return the max zoom level we have (the
    ;; calculation below might return +/-inf otherwise
    (if (and (< w 5.0) (< h 5.0))
        (max-zoom-level)
        ;; mpp -- meters per pixel
        (let ((mpp-width (/ w canvas-width))
              (mpp-height (/ h canvas-height)))
          (mpp->zoom-level (max mpp-width mpp-height))))))

;; Return a track that has fewer points than TRACK but should display OK at
;; ZOOM-LEVEL.  We drop points from TRACK such that there is a minimum
;; distance between two points.  When simplifying the track, we are carefull
;; not to drop points if the bearing of the track changes, as this will result
;; in a distorted track.
(define (simplify-track track zoom-level)

  ;; Minimum distance between points in meters (each segment will be approx 30
  ;; pixels).  Points closer together than this value will be dropped, unless
  ;; bearing changes by more than MAX-BDEV
  (define min-dist (* 30 (zoom-level->mpp zoom-level)))

  ;; Maximum bearning deviation we allow when dropping points.  Since the
  ;; track does not usually go in a straight line, the direction (bearing) of
  ;; the track will change if we skip a point.  MAX-BDEV is the maximum
  ;; direction change we allow, a point will not be dropped if the bearing
  ;; change would be greater than this value
  (define max-bdev (* 3 (/ pi 180)))

  (define (bear p1 p2)
    (map-bearing/degrees
     (point-lat p1) (point-lon p1) (point-lat p2) (point-lon p2)))

  (define (dist p1 p2)
    (map-distance/degrees
     (point-lat p1) (point-lon p1) (point-lat p2) (point-lon p2)))

  (if (< (sequence-length track) 3)
      (sequence->list track)            ; cannot simplify a short track
      (let* ((strack (sequence->stream track))
             (ntrack (list (stream-first strack)))) ; we always keep the first point
        (let loop ((start (stream-first strack))
                   (follow (stream-first (stream-rest strack)))
                   (rest (stream-rest (stream-rest strack)))
                   (bearing (bear (stream-first strack)
                                  (stream-first (stream-rest strack)))))
          (if (stream-empty? rest)
              (set! ntrack (cons follow ntrack)) ; last point
              (let* ((candidate (stream-first rest))
                     (ndist (dist start candidate))
                     (nbearing (bear start candidate)))
                (if (or (>  ndist min-dist)
                        (> (abs (- nbearing bearing)) max-bdev))
                    (begin
                      (set! ntrack (cons follow ntrack))
                      (loop follow candidate (stream-rest rest) (bear follow candidate)))
                    (begin
                      (loop start candidate (stream-rest rest) bearing))))))
        (reverse ntrack))))


;;............................................................. provides ....

(provide
 (struct-out npoint)
 (struct-out bbox)
 (struct-out tile))

(provide/contract
 (map-distance/radians (-> flonum? flonum? flonum? flonum? flonum?))
 (map-distance/degrees (-> flonum? flonum? flonum? flonum? flonum?))
 (map-bearing/radians (-> flonum? flonum? flonum? flonum? flonum?))
 (map-bearing/degrees (-> flonum? flonum? flonum? flonum? flonum?))
 (lat-lon->npoint (-> flonum? flonum? npoint?))
 (npoint->lat-lon (-> npoint? (values flonum? flonum?)))
 (track-bbox (-> sequence? bbox?))
 (bbox-center (-> bbox? (values flonum? flonum?)))
 (bbox-center/ndcs (-> bbox? npoint?))
 (bbox-size (-> bbox? (values flonum? flonum?)))
 (bbox-merge (-> bbox? bbox? bbox?))
 (bbox-extend (-> bbox? vector? bbox?))
 (tile-equal? (-> tile? tile? boolean?))
 (zoom-level->mpp (-> exact-positive-integer? flonum?))
 (mpp->zoom-level (-> flonum? exact-positive-integer?))
 (min-zoom-level (-> exact-positive-integer?))
 (max-zoom-level (-> exact-positive-integer?))
 (select-zoom-level (-> bbox? positive? positive? exact-positive-integer?))
 (simplify-track (-> sequence? exact-positive-integer? sequence?)))

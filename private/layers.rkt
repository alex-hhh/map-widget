#lang racket/base
;; SPDX-License-Identifier: LGPL-3.0-or-later
;; layers.rkt -- implementation for different elements shown on the map
;;
;; This file is part of map-widget -- A Racket GUI Widget to display maps
;; based on OpenStreetMap tiles
;;
;; Copyright (c) 2019, 2024, 2023, 2024 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require pict
         racket/class
         racket/contract
         racket/gui/base
         racket/list
         racket/match
         racket/math
         racket/sequence
         "map-util.rkt"
         "point-cloud.rkt"
         "tiles.rkt"
         "utilities.rkt")

;; Draw the BOUNDING-BOX onto the device DC at the specified ZOOM-LEVEL.  This
;; is intended for debugging purposes -- an outline of the bounding box is
;; drawn plus a circle in the middle of it.  The pen and brush are not
;; changed, so they can be set in before calling this method to the desired
;; values.
;;
;; NOTE: this function assumes that the map origin has been set up correctly
;; see `with-origin`
;;
(define (draw-bounding-box dc bounding-box zoom-level)

  (define debug-pen
    (send the-pen-list find-or-create-pen (make-object color% 86 13 24) 2 'solid))

  (send dc set-pen debug-pen)
  (send dc set-brush
        (send the-brush-list find-or-create-brush "white" 'transparent))

  (define (get-center zoom-level)
    (let ((max-coord (* tile-size (expt 2 zoom-level)))
          (center/ndcs (bbox-center/ndcs bounding-box)))
      (values (* (npoint-x center/ndcs) max-coord)
              (* (npoint-y center/ndcs) max-coord))))

  (let-values (([cx cy] (get-center zoom-level)))
    (send dc draw-ellipse (+ cx -10) (+ cy -10) 20 20))
  (match-define (bbox max-lat max-lon min-lat min-lon) bounding-box)
  (let ((max-coord (* tile-size (expt 2 zoom-level)))
        (map1 (lat-lon->npoint max-lat min-lon))
        (map2 (lat-lon->npoint min-lat max-lon)))
    (let ((x1 (* (npoint-x map1) max-coord))
          (y1 (* (npoint-y map1) max-coord))
          (x2 (* (npoint-x map2) max-coord))
          (y2 (* (npoint-y map2) max-coord)))
      (send dc draw-rectangle x1 y1 (- x2 x1) (- y2 y1)))))


;;............................................................... layers ....

;; A layer represents an element drawn on the map, such as tracks, markers or
;; points.  A layer has a name, a z-order (which determines its position
;; relative to other layers), a bounding box and also provides a "draw" method
;; which draws the layer onto the map.
(define layer<%>
  (interface ()

    ;; Get/Set the administrator, that is the map-impl% instance, which
    ;; manages this layer.
    set-admin
    get-admin

    get-name
    get-zorder
    set-zorder
    get-bounding-box

    clone                     ; creates a copy of this layer with all its data

    draw                                ; draw the layer onto the map

    on-zoom-level-change

    ;; Called by the admin for mouse events -- the layer must register itself
    ;; using the admin's register-for-mouse-events method in order to receive
    ;; mouse events...
    on-mouse-event
    ))

(define (sort-layers-by-zorder layers)
  (sort layers > #:key (lambda (l) (send l get-zorder))))

;; Convenience "root" class for layers, providing reasonable defaults for the
;; different layer methods in the layer<%> interface.
(define layer%
  (class object%
    (init-field name
                [zorder 0.5]
                [admin #f])
    (super-new)

    (define/public (get-admin)
      admin)

    (define/public (set-admin a)
      ;; Unregister this layer from previous one...
      (unless (equal? admin a)
        (and admin (send admin unregister-for-mouse-events this)))
      (set! admin a)
      a)

    (define/public (get-name)
      name)

    (define/public (get-zorder)
      zorder)

    (define/public (set-zorder z)
      (set! zorder z)
      (when admin
        (send admin on-zorder-changed)))

    (define/public (on-zoom-level-change zl)
      (void))

    (define/public (on-mouse-event dc x y editorx editory event)
      ;; Return #f by default to indicate that we didn't handle the event.
      #f)

    ))



;;......................................................... lines-layer% ....

(define (point-lat p) (vector-ref p 0))
(define (point-lon p) (vector-ref p 1))

;; Construct a dc-path% that draws the TRACK at ZOOM-LEVEL
(define (waypoints->dc-path waypoints zoom-level)
  (define max-coord (* tile-size (expt 2 zoom-level)))

  (define (p->pixel p)
    (let ((p (lat-lon->npoint (point-lat p) (point-lon p))))
      (values (* max-coord (npoint-x p))
              (* max-coord (npoint-y p)))))

  (let ((path (new dc-path%)))
    (unless (null? waypoints)
      (let-values (([start-x start-y] (p->pixel (car waypoints))))
        (send path move-to start-x start-y))
      (for ((p (in-list (cdr waypoints))))
        (let-values (([px py] (p->pixel p)))
          (send path line-to px py))))
    path))

;; Represent a GPS track that can be drawn on a dc<%> at different zoom
;; levels.  A track also has a group which is an integer, the map-impl% will
;; treat all tracks within a group identically with respect to draw order and
;; pen color.
(define line%
  (class object%
    (init-field waypoints)
    (super-new)

    (define bbox #f)
    (define debug?
      (get-pref 'map-widget:draw-track-bounding-box (lambda () #f)))
    (define paths-by-zoom-level (make-hash))

    (define/private (get-dc-path zoom-level)
      (let ((dc-path (hash-ref paths-by-zoom-level zoom-level #f)))
        (unless dc-path
          ;; no dc-path at this zoom level, create one now
          (let ((strack (simplify-track waypoints zoom-level)))
            (set! dc-path (waypoints->dc-path strack zoom-level))
            (hash-set! paths-by-zoom-level zoom-level dc-path)))
        dc-path))

    ;; NOTE: this function assumes that the map origin has been set up correctly
    ;; see `with-origin`
    (define/public (draw dc zoom-level)
      (let ((path (get-dc-path zoom-level)))
        (send dc draw-path path 0 0))
      (when debug?
        (draw-bounding-box dc bbox zoom-level)))

    (define/public (get-bounding-box)
      (unless bbox
        (set! bbox (track-bbox waypoints)))
      bbox)

    ))

(define (default-lines-pen)
  (send the-pen-list find-or-create-pen (make-object color% 226 34 62) 3 'solid 'round 'round))
(define (default-lines-zorder)
  0.8)

;; A layer that draws a collection of lines, a list of LAT/LON waypoints or
;; GPS tracks
(define lines-layer%
  (class* layer% (layer<%>)
    (init [zorder (default-lines-zorder)])
    (init-field
     [lines '()]
     [pen (default-lines-pen)])
    (super-new [zorder zorder])
    (inherit get-admin)

    (define the-bbox #f)
    (define transparent-brush
      (send the-brush-list find-or-create-brush "white" 'transparent))

    (define/public (get-bounding-box)
      (unless the-bbox
        (set! the-bbox
              (for/fold ([outer #f])
                        ([track (in-list lines)])
                (let ([bb (send track get-bounding-box)])
                  (if outer (bbox-merge outer bb) bb)))))
      the-bbox)

    (define/public (clone)
      (new lines-layer%
           [name (send this get-name)]
           [zorder (send this get-zorder)]
           [lines lines]
           [pen pen]))

    (define/public (draw dc the-zoom-level)
      (send dc set-pen pen)
      (send dc set-brush transparent-brush)
      (for ([line (in-list lines)])
        (send line draw dc the-zoom-level)))

    (define/public (add-line waypoints)
      (define l (new line% [waypoints waypoints]))
      (set! lines (append lines (list l)))
      (set! the-bbox #f)
      (let ([a (get-admin)])
        (when a
          (send a refresh #:maybe-resize #t))))

    (define/public (set-pen p)
      (set! pen p)
      (let ([a (get-admin)])
        (when a
          (send a refresh))))

    (define/public (clear)
      (set! lines '())
      (set! the-bbox #f))

    ))

;; Create a new lines-layer% with NAME, LINES and pen, brush and z-order.  The
;; layer needs to be added to the map in order to be shown.
(define (lines-layer
         name
         lines
         #:pen (pen (default-lines-pen))
         #:zorder (zorder (default-lines-zorder)))
  (new lines-layer%
       [name name]
       [lines (for/list ([wps (in-list lines)])
                (new line% [waypoints wps]))]
       [zorder zorder]
       [pen pen]))

;; Convenience function to create a lines-layer% with a single track or
;; waypoints
(define (line-layer
         name
         waypoints
         #:pen (pen (default-lines-pen))
         #:zorder (zorder (default-lines-zorder)))
  (new lines-layer%
       [name name]
       [lines (list (new line% [waypoints waypoints]))]
       [zorder zorder]
       [pen pen]))


;;....................................................... markers-layer% ....

;; Draw a label (marker) at POS (a `map-point`), at ZOOM-LEVEL.  Label is the
;; text to display, direction is 1 for the text to be displayed to the right,
;; and -1 for the text to be displayed on the left of the marker, while color
;; is the color of the label.
;;
;; NOTE: this function assumes that the map origin has been set up correctly
;; see `with-origin`
;;
(define (draw-label dc pos zoom-level label direction color)

  (send dc set-pen
        (send the-pen-list find-or-create-pen color 2 'solid))
  (send dc set-font
        (send the-font-list find-or-create-font 10 'default 'normal 'bold))
  (send dc set-brush
        (send the-brush-list find-or-create-brush
              (make-color
               (send color red)
               (send color green)
               (send color blue)
               0.7)
              'solid))
  (send dc set-text-foreground "white")

  ;; NOTE: we assume that the dc origin has been corectly set up!
  (let* ((max-coord (* tile-size (expt 2 zoom-level)))
         (x (* (npoint-x pos) max-coord))
         (y (* (npoint-y pos) max-coord)))
    (let-values (([w h b e] (send dc get-text-extent label)))
      (let ((arrow-length 30)
            (text-spacing 2))
        (let ((label-baseline-x (+ x (* direction arrow-length)))
              (label-baseline-y (+ y (- arrow-length)))
              (label-length (+ w text-spacing text-spacing))
              (label-height (+ h text-spacing text-spacing)))
          (send dc draw-line x y label-baseline-x label-baseline-y)
          (send dc draw-line
                label-baseline-x label-baseline-y
                (+ label-baseline-x (* direction label-length))
                label-baseline-y)
          (send dc set-pen
                (send the-pen-list find-or-create-pen "black" 1 'transparent))
          (let ((rectangle-y (- label-baseline-y label-height))
                (rectangle-x (if (> direction 0)
                                 label-baseline-x
                                 (- label-baseline-x label-length))))
            (send dc draw-rectangle
                  rectangle-x rectangle-y
                  label-length label-height)
            (send dc draw-text label
                  (+ rectangle-x text-spacing)
                  (+ rectangle-y text-spacing))))))))

;; Represents a labeled marker drawn on the map at POS (a GPS coordinate) with
;; TEXT and COLOR.  Direction is 1 if the text is drawn on the right and -1 if
;; it is drawn on the left.
(define marker%
  (class object%
    (init-field pos text direction color)
    (super-new)

    (define point (lat-lon->npoint (point-lat pos) (point-lon pos)))

    (define/public (draw dc zoom-level)
      (draw-label dc point zoom-level text direction color))

    (define/public (get-position) pos)

    ))

(define (default-markers-zorder)
  0.7)

;; A layer that draws a collection of markers on the map
(define markers-layer%
  (class* layer% (layer<%>)
    (init [markers '()]
          [zorder (default-markers-zorder)])
    (super-new [zorder zorder])
    (inherit get-admin)

    (field
     [the-markers
      (for/list ([m (in-list markers)])
        (match-define (list pos text direction color) m)
        (new marker%
             [pos pos]
             [text text]
             [direction direction]
             [color color]))]
     [the-bbox
      (if (null? the-markers)
          #f
          (for/fold ([outer (bbox-from-position (send (car the-markers) get-position))])
                    ([marker (in-list (cdr the-markers))])
            (bbox-extend outer (send marker get-position))))])

    (define/public (clone)
      (define c (new markers-layer%
                     [name (send this get-name)]
                     [zorder (send this get-zorder)]))
      (set-field! the-markers c the-markers)
      (set-field! the-bbox c the-bbox)
      c)

    (define/public (draw dc zoom-level)
      (for ([m (in-list the-markers)])
        (send m draw dc zoom-level)))

    (define/public (get-bounding-box)
      the-bbox)

    (define/public (add-marker pos text direction color)
      (define m (new marker%
                     [pos pos]
                     [text text]
                     [direction direction]
                     [color color]))
      (set! the-markers (append the-markers (list m)))
      (set! the-bbox
            (if the-bbox
                (bbox-extend the-bbox (send m get-position))
                (bbox-from-position (send m get-position))))
      (let ([a (get-admin)])
        (when a
          (send a refresh #:maybe-resize #t))))

    (define/public (clear)
      (set! the-markers '())
      (set! the-bbox #f))

    ))

;; Create a new markers layer.  Note that the layer needs to be added to a map
;; for it to be shown.  Each marker is a (list pos text direction color),
;; where pos is a vector of LAT/LON, and direction is -1 or 1 for left or
;; right.
(define (markers-layer
         name
         markers
         #:zorder (zorder (default-markers-zorder)))
  (new markers-layer%
       [name name]
       [zorder zorder]
       [markers markers]))


;;........................................................ points-layer% ....

(define (default-points-pen)
  (send the-pen-list find-or-create-pen "black" 0.5 'solid))
(define (default-points-brush)
  (send the-brush-list find-or-create-brush (make-object color% 51 187 238) 'solid))
(define (default-points-hlpen)
  (send the-pen-list find-or-create-pen "black" 0.5 'solid))
(define (default-points-hlbrush)
  (send the-brush-list find-or-create-brush (make-object color% 238 51 119) 'solid))
(define (default-points-zorder)
  0.6)
(define (default-points-size)
  10)
(define (default-points-hlsize)
  15)

;; A layer that shows a collection of POINTS, a list of LAT/LON vectors, shown
;; as "dots" or "circles", on the map.  The user can also provide a "hover"
;; function which allows showing arbitrary data when the user hovers over a
;; point.
(define points-layer%
  (class* layer% (layer<%>)
    (init [points '()]
          [zorder (default-points-zorder)])

    (init-field

     ;; Pen, Brush and Size (diameter) used to draw the points
     [pen (default-points-pen)]
     [brush (default-points-brush)]
     [size (default-points-size)]

     ;; Pen, Brush and Size (diameter) used to draw the point over which the
     ;; mouse is hovering.
     [hlpen (default-points-hlpen)]
     [hlbrush (default-points-hlbrush)]
     [hlsize (default-points-hlsize)]

     ;; Callback invoked to show a pict for the point the mouse is hovering
     ;; over... It should return #f (to show nothing) or a `pict` object.
     [hover-callback (lambda (_point-index) #f)])

    (super-new [zorder zorder])
    (inherit get-admin)

    (field
     [the-points
      (for/list ([p (in-list points)])
        (lat-lon->npoint (point-lat p) (point-lon p)))]
     [the-bbox
      (if (null? points)
          #f
          (for/fold ([bb (bbox-from-position (car points))])
                    ([p (in-list (cdr points))])
            (bbox-extend bb p)))])

    (define/public (clone)
      (define c (new points-layer%
                     [name (send this get-name)]
                     [zorder (send this get-zorder)]
                     [pen pen]
                     [brush brush]))
      (set-field! the-points c the-points)
      (set-field! the-bbox c the-bbox)
      c)

    (define/override (set-admin a)
      (super set-admin a)
      (when a
        (send a register-for-mouse-events (send this get-name))))

    (define mouse-dx #f)
    (define mouse-dy #f)
    (define mouse-hover-timer
      (new timer% [notify-callback (lambda () (on-mouse-hover))]))

    (define closest-point-x #f)
    (define closest-point-y #f)
    (define closest-point-index #f)

    ;; Determine the point that is closed to the current mouse location
    ;; (MOUSE-DX, MOUSE-DY).  Updates CLOSEST-POINT-X, CLOSEST-POINT-Y and
    ;; CLOSEST-POINT-INDEX.
    (define/private (on-mouse-hover)
      (define a (get-admin))
      (when (and a mouse-dx mouse-dy)
        (define-values (ox oy) (send a get-origin))
        (define mx (+ mouse-dx ox))
        (define my (+ mouse-dy oy))
        (define max-coord (* tile-size (expt 2 (send a zoom-level))))

        (define-values (cp-x cp-y cp-index cp-distance)
          (for/fold ([cp-x #f]
                     [cp-y #f]
                     [cp-index #f]
                     [cp-distance +inf.0])
                    ([p (in-list the-points)]
                     [i (in-naturals)])
            (let ((x (* (npoint-x p) max-coord))
                  (y (* (npoint-y p) max-coord)))
              (define d (+ (sqr (- x mx)) (sqr (- y my))))
              (if (< d cp-distance)
                  (values x y i d)
                  (values cp-x cp-y cp-index cp-distance)))))

        (if (< cp-distance (* 1.1 (sqr (/ size 2))))
            (begin
              (set! closest-point-x cp-x)
              (set! closest-point-y cp-y)
              (set! closest-point-index cp-index)
              (send a refresh))
            (when closest-point-index   ; avoid too many refreshes
              (set! closest-point-x #f)
              (set! closest-point-y #f)
              (set! closest-point-index #f)
              (send a refresh)))))

    (define/override (on-mouse-event _dc x y _editorx _editory event)
      ;; Return #f by default to indicate that we didn't handle the event.
      (set! mouse-dx (- (send event get-x) x))
      (set! mouse-dy (- (send event get-y) y))
      (send mouse-hover-timer start 20 #t)
      #f)

    (define/public (draw dc zoom-level)
      (send dc set-pen pen)
      (send dc set-brush brush)
      (define max-coord (* tile-size (expt 2 zoom-level)))
      (define offset (/ size 2))
      (for ([p (in-list the-points)])
        (let ((x (* (npoint-x p) max-coord))
              (y (* (npoint-y p) max-coord)))
          (send dc draw-ellipse (- x offset) (- y offset) size size)))

      (when (and closest-point-x closest-point-y closest-point-index)
        (define-values (cpx cpy) (values closest-point-x closest-point-y))
        (define hloffset (/ hlsize 2))
        (send dc set-pen hlpen)
        (send dc set-brush hlbrush)
        (send dc draw-ellipse (- cpx hloffset) (- cpy hloffset) hlsize hlsize)
        (define p (hover-callback closest-point-index))
        (when p
          (define-values (ox oy) (send dc get-origin))
          (define-values (w h) (send (get-admin) get-size))
          ;; screen coordinates of picture
          (define-values (sx sy) (values (+ cpx ox) (+ cpy oy)))
          (define-values (pw ph) (values (pict-width p) (pict-height p)))

          ;; Place the tooltip around the mouse location such that it shows on
          ;; the screen.
          (cond ((and (< (+ sx pw) w)
                      (< (+ sy ph) h))
                 (draw-pict p dc (+ cpx hlsize) (+ cpy hlsize)))
                ((< (+ sx pw) w)
                 (draw-pict p dc (+ cpx hlsize ) (- cpy hlsize ph)))
                ((< (+ sy ph) h)
                 (draw-pict p dc (- cpx hlsize pw) (+ cpy hlsize)))
                (else
                 (draw-pict p dc (- cpx hlsize pw) (- cpy hlsize ph)))))))

    (define/public (get-bounding-box)
      the-bbox)
    ))

(define (points-layer name
                      points
                      #:zorder (zorder (default-points-zorder))
                      #:pen (pen (default-points-pen))
                      #:brush (brush (default-points-brush))
                      #:size (size (default-points-size))
                      #:hlpen (hlpen (default-points-hlpen))
                      #:hlbrush (hlbrush (default-points-hlbrush))
                      #:hlsize (hlsize (default-points-hlsize))
                      #:hover-callback (hover-callback (lambda (_point-index) #f)))
  (new points-layer%
       [name name]
       [zorder zorder]
       [points points]
       [pen pen]
       [brush brush]
       [size size]
       [hlpen hlpen]
       [hlbrush hlbrush]
       [hlsize hlsize]
       [hover-callback hover-callback]))


;;................................................... point-cloud-layer% ....

(define (default-point-cloud-zorder)
  0.9)

;; A point-cloud layer shows a large amount (millions) of points on a map,
;; grouped together and colored using a color map to show density of data
;; around a location.
(define point-cloud-layer%
  (class* layer% (layer<%>)
    (init [zorder (default-point-cloud-zorder)]
          [color-map #f])
    (super-new [zorder zorder])
    (inherit get-admin)

    (field [the-point-cloud-color-map color-map]
           [the-point-cloud #f])

    (define (refresh)
      (define admin (get-admin))
      (when admin
        (send admin refresh)))

    (define/public (clone)
      (define c (new point-cloud-layer%
                     [name (send this get-name)]
                     [zorder (send this get-zorder)]))
      (set-field! the-point-cloud-color-map c the-point-cloud-color-map)
      (set-field! the-point-cloud c the-point-cloud)
      c)

    (define/public (draw dc zoom-level)
      (when the-point-cloud
        (send the-point-cloud draw dc zoom-level)
        (define-values (c t) (send the-point-cloud get-point-count))
        (when (< c t)
          (send (get-admin) refresh #:delay 500))))

    (define/public (get-point-count)
      (if the-point-cloud
          (send the-point-cloud get-point-count)
          (values 0 0)))

    (define/public (set-color-map cm)
      (set! the-point-cloud-color-map cm)
      (when the-point-cloud
        (send the-point-cloud set-color-map cm)
        (let ([a (get-admin)])
          (when a
            (send a refresh)))))

    (define/public (get-bounding-box)
      (and the-point-cloud (send the-point-cloud get-bounding-box)))

    (define/public (add-points points #:format fmt)
      (unless the-point-cloud
        (set! the-point-cloud
              (new point-cloud%
                   [color-map the-point-cloud-color-map]
                   [refresh-callback refresh])))
      (send the-point-cloud add-points points #:format fmt)
      (let ([a (get-admin)])
        (when a
          (send a refresh #:delay 500))))

    (define/public (clear)
      (set! the-point-cloud #f)
      (let ([a (get-admin)])
          (when a
            (send a refresh))))

    ))

(define (point-cloud-layer
         name
         #:zorder (zorder (default-point-cloud-zorder))
         #:color-map (color-map #f))
  (new point-cloud-layer%
       [name name]
       [zorder zorder]
       [color-map color-map]))


;;.............................................. current-location-layer% ....

(define (default-current-location-zorder)
  0.2)
(define (default-current-location-pen)
  (send the-pen-list find-or-create-pen (make-color 68 114 196) 5 'solid))
(define (default-current-location-brush)
  (send the-brush-list find-or-create-brush (make-color 68 114 196 0.5) 'solid))
(define (default-current-location-size)
  24)

;; Shows a single location on the map, can be used to track a "current"
;; location based on some other selection, e.g. while hovering over a plot.
;; The user needs to update the location using the `current-location` member
;; and the map
(define current-location-layer%
  (class* layer% (layer<%>)
    (init [zorder (default-current-location-zorder)])
    (init-field
     (pen (default-current-location-pen))
     (brush (default-current-location-brush))
     (size (default-current-location-size))
     ;; When #t, the map is panned so that the current-location is in the
     ;; center of the view, this panning is animated
     [should-track-current-location #f])
    (super-new [zorder zorder])
    (inherit get-admin)

    (field
     ;; A (vector lat lon) where we draw a marker representing the "current
     ;; location"
     [the-current-location #f])

    ;; Set or get the location shown as a vector of "LAT/LON"
    (public current-location)
    (define current-location
      (case-lambda
        (()
         the-current-location)
        ((pos)
         (set! the-current-location pos)
         (on-current-location-updated))))

    ;; Set or get the "track" flag -- when #t, the map is panned so that the
    ;; current location is visible on the screen.
    (public track-current-location)
    (define track-current-location
      (case-lambda
        (() should-track-current-location)
        ((flag)
         (set! should-track-current-location flag)
         (on-current-location-updated))))

    ;; The X, Y coordinates of the current location (in canvas coordinates).
    ;; Updated by `on-current-location-updated`
    (define last-current-location-x #f)
    (define last-current-location-y #f)

    ;; Timer to schedule a map drag event to pan the current location in view
    (define auto-drag-map-timer
      (new timer% [notify-callback (lambda () (on-current-location-updated))]))

    (define/public (clone)
      (define c (new current-location-layer%
                     [name (send this get-name)]
                     [zorder (send this get-zorder)]
                     [pen pen]
                     [brush brush]
                     [size size]
                     [should-track-current-location should-track-current-location]))
      (set-field! the-current-location c the-current-location)
      c)

    (define/override (set-admin a)
      (super set-admin a)
      (when a
        (on-current-location-updated)))

    (define/public (draw dc zoom-level)
      ;; Draw the current location marker, as set by
      ;; `on-current-location-updated'
      (when (and last-current-location-x last-current-location-y)
        (send dc set-pen pen)
        (send dc set-brush brush)
        (define offset (/ size 2))
        (send dc draw-ellipse
              (- last-current-location-x offset)
              (- last-current-location-y offset)
              size size)))

    (define/public (get-bounding-box)
      #f)

    (define/override (on-zoom-level-change zl)
      (set! last-current-location-x #f)
      (set! last-current-location-y #f)
      (on-current-location-updated))

    ;; Called when the current location has been updated, handles redisplay of
    ;; the current location as well as auto-dragging the map, if this is
    ;; enabled.
    (define/private (on-current-location-updated)

      (define admin (get-admin))

      (when admin

        ;; The current location has been cleared, refresh the map
        (when (and (not the-current-location)
                   (or last-current-location-x last-current-location-y))
          (set! last-current-location-x #f)
          (set! last-current-location-x #f)
          (let ([admin (get-admin)])
            (when admin
              (send admin refresh))))

        (when the-current-location
          (let-values ([(origin-x origin-y) (send admin get-origin)]
                       [(width height) (send admin get-size)])
            (let* ((max-coord (send admin get-max-coord))
                   (point (lat-lon->npoint
                           (point-lat the-current-location) (point-lon the-current-location)))
                   (px (* max-coord (npoint-x point)))
                   (py (* max-coord (npoint-y point)))
                   (cx (+ origin-x (/ width 2)))
                   (cy (+ origin-y (/ height 2)))
                   (dx (- cx px))
                   (dy (- cy py))
                   (need-refresh? #f))
              ;; We only need a refresh if the current location moved at least
              ;; one pixel on the screen.
              (set! need-refresh?
                    (or (not last-current-location-x)
                        (not last-current-location-y)
                        (>= (abs (- last-current-location-x px)) 1.0)
                        (>= (abs (- last-current-location-y py)) 1.0)))
              (when need-refresh?
                (send admin refresh)
                ;; Only update this if we need to refresh -- otherwise we can
                ;; creep out in small increments and never notice it!
                (set! last-current-location-x px)
                (set! last-current-location-y py))

              (when should-track-current-location
                (define auto-drag-map
                  (cond
                    ((or (> (abs dx) (/ width 8)) (> (abs dy) (/ height 8))) #t)
                    ((and (< (abs dx) 1) (< (abs dy) 1)) #f)
                    (#t #f)))
                (when auto-drag-map
                  (send admin drag-map (* dx 0.1) (* dy 0.1))
                  (send auto-drag-map-timer start 100 #t))))))))

    ))

(define (current-location-layer
         name
         #:track-current-location? (track-current-location? #t)
         #:zorder (zorder (default-current-location-zorder))
         #:pen (pen (default-current-location-pen))
         #:brush (brush (default-current-location-brush))
         #:size (size (default-current-location-size)))
  (new current-location-layer%
       [name name]
       [should-track-current-location track-current-location?]
       [zorder zorder]
       [pen pen]
       [brush brush]
       [size size]))


;;.................................................... map-legend-layer% ....

;; an internal layer to display the map legend
(define map-legend-layer%
  (class* layer% (layer<%>)
    (init [zorder 0.0])
    (super-new [zorder zorder])
    (inherit get-admin)

    (define/public (clone)
      (define c (new map-legend-layer%
                     [name (send this get-name)]
                     [zorder (send this get-zorder)]))
      c)

    (define/public (draw dc zoom-level)
      (define admin (get-admin))
      (when admin
        (define-values (x y) (send admin get-origin))
        (define-values (w h) (send admin get-size))
        (draw-map-legend dc x y w h zoom-level)))

    (define/public (get-bounding-box)
      #f)

    ;; Legend color, pen and font -- these are used to draw the map legend.
    (define legend-color (make-object color% 86 13 24))
    (define legend-pen
      (send the-pen-list find-or-create-pen legend-color 2 'solid))
    (define legend-font
      (send the-font-list find-or-create-font 8 'default 'normal 'normal))

    ;; Map distance markers for each zoom level, for metric distances
    (define legend-distance-metric
      (list
       (list 1 8000000 "8000 km")
       (list 2 4000000 "4000 km")
       (list 3 2000000 "2000 km")
       (list 4 1000000 "1000 km")
       (list 5 500000 "500 km")
       (list 6 250000 "250 km")
       (list 7 100000 "100 km")
       (list 8  50000 "50 km")
       (list 9  25000 "25 km")
       (list 10 15000 "15 km")
       (list 11 10000 "10 km")
       (list 12  5000 "5 km")
       (list 13  2000 "2 km")
       (list 14  1000 "1 km")
       (list 15   500 "500 m")
       (list 16   200 "200 m")
       (list 17   100 "100 m")
       (list 18    50 "50 m")))

    ;; Map distance markers for each zoom level, for statute distances
    (define legend-distance-statute
      (list
       (list 1 12874752.0 "8000 mi")
       (list 2 6437376.0 "4000 mi")
       (list 3 3218688.0 "2000 mi")
       (list 4 1609344.0 "1000 mi")
       (list 5 804672.0 "500 mi")
       (list 6 402336.0 "250 mi")
       (list 7 160934.4 "100 mi")
       (list 8  80467.20 "50 mi")
       (list 9  32186.88 "20 mi")
       (list 10 16093.44 "10 mi")
       (list 11  8046.72 "5 mi")
       (list 12  3218.68 "2 mi")
       (list 13  1609.344 "1 mi")
       (list 14   804.67 "0.5 mi")
       (list 15   457.20 "500 yd")
       (list 16   182.88 "200 yd")
       (list 17    91.44 "100 yd")
       (list 18    45.72 "50 yd")))

    ;; Draw the map legend on the device context DC, assuming the map is drawn
    ;; starting at DX, DY and has a WIDTH and HEIGHT.  The legend is drawn for
    ;; the specific ZOOM-LEVEL.
    ;;
    ;; Note that this method does not assume that the map is drawn onto the
    ;; entire device context, but only in the rectangle defined by DX, DY,
    ;; WIDTH and HEIGHT.
    (define/private (draw-map-legend dc dx dy width height zoom-level)
      (define-values (metric-distance metric-label)
        (let ((entry (assq zoom-level legend-distance-metric)))
          (if entry
              (values (second entry) (third entry))
              (values 1000 "1 km"))))
      (define-values (statute-distance statute-label)
        (let ((entry (assq zoom-level legend-distance-statute)))
          (if entry
              (values (second entry) (third entry))
              (values 1609.344 "1 mi"))))
      (define mlabel
        (let ((backlog-size (get-download-backlog)))
          (if (> backlog-size 0)
              (format "~a  (ZL ~a; BL ~a)" metric-label zoom-level backlog-size)
              (format "~a  (ZL ~a)" metric-label zoom-level))))
      (define slabel statute-label)
      (define mdist (/ metric-distance (zoom-level->mpp zoom-level)))
      (define sdist (/ statute-distance (zoom-level->mpp zoom-level)))
      (define-values (ox oy) (values 10 10))
      (define-values (cw ch) (values width height))
      (send dc set-brush
            (send the-brush-list find-or-create-brush
                  (make-color 255 255 255 0.7) 'solid))
      (send dc set-pen
            (send the-pen-list find-or-create-pen "white" 1 'transparent))
      (send dc set-font legend-font)
      (send dc set-text-foreground legend-color)

      (define Y (- (+ ch dy) oy 10))
      (define X (+ ox dx))

      (let-values (((w h x y) (send dc get-text-extent (tile-copyright-string) legend-font #t)))
        (send dc draw-rectangle (- (+ cw dx) ox 5 w) (- (+ ch dy) oy y 5 h) (+ w 5 5) (+ h 5 5))
        (send dc draw-text (tile-copyright-string) (- (+ cw dx) ox w) (- (+ ch dy -10) h))

        ;; use the height of the copyright string to determine the height of the
        ;; legend rectangle.
        (send dc draw-rectangle (- X 5) (- Y 20)
              (+ (max mdist sdist) 5 5) (+ (* 2 (+ h 3)) 5)))

      (send dc set-pen legend-pen)

      (send dc draw-line X Y (+ X (max mdist sdist)) Y)
      (send dc draw-line X Y X (- Y 10))
      (send dc draw-line X Y X (+ Y 10))
      (send dc draw-line (+ X mdist) Y (+ X mdist) (- Y 10))
      (send dc draw-line (+ X sdist) Y (+ X sdist) (+ Y 10))

      (let-values (([w h b e] (send dc get-text-extent mlabel)))
        (let ((tx (+ X 3))
              (ty (- Y 3 h)))
          (send dc draw-text mlabel tx ty)))

      (let-values (([w h b e] (send dc get-text-extent slabel)))
        (let ((tx (+ X 3))
              (ty (+ Y 3)))
          (send dc draw-text slabel tx ty))))

    ))


;;............................................................. provides ....

(provide
 draw-bounding-box
 sort-layers-by-zorder
 layer<%>
 lines-layer%
 markers-layer%
 points-layer%
 point-cloud-layer%
 current-location-layer%
 map-legend-layer%)

(provide/contract
 (line-layer
  (->* ((or/c symbol? integer?)
        (sequence/c (vector/c real? real?)))
       (#:pen (is-a?/c pen%)
        #:zorder (between/c 0 1))
       (is-a?/c lines-layer%)))

 (lines-layer
  (->* ((or/c symbol? integer?)
        (listof (sequence/c (vector/c real? real?))))
       (#:pen (is-a?/c pen%)
        #:zorder (between/c 0 1))
       (is-a?/c lines-layer%)))

 (markers-layer
  (->* ((or/c symbol? integer?)
        (listof (list/c (vector/c real? real?)
                        string?
                        (or/c -1 1)
                        (or/c string? (is-a?/c color%)))))
       (#:zorder (between/c 0 1))
       (is-a?/c markers-layer%)))

 (points-layer
  (->* ((or/c symbol? integer?) (listof (vector/c real? real?)))
       (#:zorder (between/c 0 1)
        #:pen (is-a?/c pen%)
        #:brush (is-a?/c brush%)
        #:size (>/c 0)
        #:hlpen (is-a?/c pen%)
        #:hlbrush (is-a?/c brush%)
        #:hlsize (>/c 0)
        #:hover-callback (-> exact-nonnegative-integer? (or/c #f pict?)))
       (is-a?/c points-layer%)))

 (point-cloud-layer
  (->* ((or/c symbol? integer?))
       (#:zorder (between/c 0 1)
        #:color-map (or/c #f (listof (list/c real? real? real?))))
       (is-a?/c point-cloud-layer%)))

 (current-location-layer
  (->* ((or/c symbol? integer?))
       (#:track-current-location? boolean?
        #:zorder (between/c 0 1)
        #:pen (is-a?/c pen%)
        #:brush (is-a?/c brush%))
       (is-a?/c current-location-layer%))))

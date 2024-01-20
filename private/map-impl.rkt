#lang racket/base
;; SPDX-License-Identifier: LGPL-3.0-or-later
;; map-impl.rkt -- map implementation, contains drawing code and keyboard and
;; event handling.
;;
;; This file is part of map-widget -- A Racket GUI Widget to display maps
;; based on OpenStreetMap tiles
;;
;; Copyright (c) 2019, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require
 racket/math
 racket/class
 racket/gui/base
 racket/match
 "utilities.rkt"          ; for get-pref
 "map-util.rkt"
 "tiles.rkt"
 "layers.rkt")

(provide map-impl%)

;; Bitmap to draw when we don't receive a tile
(define empty-bmp (make-bitmap tile-size tile-size #f))

;; Cursors for the mouse is over the map widget -- the hand-cursor is used
;; when the map is dragged around.
(define hand-cursor (make-object cursor% 'hand))
(define arrow-cursor (make-object cursor% 'arrow))

;; Set the draw context on the device context DC such that the origin is at
;; ORIGIN-X and ORIGIN-Y than execute THUNK, the original origin is restored
;; at the end.
(define (with-draw-context dc thunk)
  (let ([old-smoothing (send dc get-smoothing)]
        [old-pen (send dc get-pen)]
        [old-brush (send dc get-brush)]
        [old-font (send dc get-font)]
        [old-text-fg (send dc get-text-foreground)]
        [old-text-bg (send dc get-text-background)])
    (dynamic-wind
      (lambda ()
        (send dc set-smoothing 'smoothed))
      thunk
      (lambda ()
        (send dc set-smoothing old-smoothing)
        (send dc set-pen old-pen)
        (send dc set-brush old-brush)
        (send dc set-font old-font)
        (send dc set-text-foreground old-text-fg)
        (send dc set-text-background old-text-bg)))))

(define (with-origin dc origin-x origin-y thunk)
  (let-values (([ox oy] (send dc get-origin)))
    (dynamic-wind
      (lambda ()
        (send dc set-origin (- origin-x) (- origin-y)))
      thunk
      (lambda ()
        (send dc set-origin ox oy)))))

;; Set a clipping rect at X,Y,WIDTH and HEIGHT onto the device context DC,
;; than execute THUNK.  The original clipping rect is restored at the end.
(define (with-clipping-rect dc x y width height thunk)
  (let ([old-clipping-region (send dc get-clipping-region)])
    (dynamic-wind
      (lambda () (send dc set-clipping-rect x y width height))
      thunk
      (lambda () (send dc set-clipping-region old-clipping-region)))))

;; A timer which does not restart when it is already running.  Calling `start`
;; on a `timer%` class will reset the alarm interval and, if `start` is called
;; repeteadly, the timer will never notify.  This class changes that behavior,
;; such that, calling `start` when the timer is already running it has no
;; effect, so the timer will eventually notify even if `start` is called
;; repeteadly.
;;
;; The implementation only supports one-off timers, where `just-once?` is #f,
;; although it could be extended for repeat timers...
(define one-shot-timer%
  (class timer%
    (init notify-callback)
    (super-new [notify-callback notify-callback]
               [just-once? #t]
               [interval #f])

    ;; NOTE: timer is initially not running, since we pass #f to `interval`
    ;; for `super-new`, so we can initialize this to #f
    (define running? #f)

    (define/override (start msec [just-once? #f])
      (unless running?
        (set! running? #t)
        ;; Ignore `just-once?` and set it to #t
        (super start msec #t)))

    (define/override (stop)
      (set! running? #f)
      (super stop))

    (define/override (notify)
      (set! running? #f)
      (super notify))))

;; Map implementation -- serves as the implementation class for canvas% and
;; snip% based maps.  It implements the map drawing as well as event handling
;; code, but relies on the "outer" classes to call the right methods.
(define map-impl%
  (class object%
    (init [zoom 1])
    (init-field [width 300] [height 200]
                [request-refresh (lambda () (void))]
                [position #f]
                [on-zoom-level-change (lambda (zl) (void))])
    (super-new)

    (define debug? (get-pref 'map-widget:draw-map-bounding-box (lambda () #f)))

    ;; Used by {begin,end}-edit-sequence to prevent the map from being
    ;; refreshed when several operations are done at once.  Map is refreshed
    ;; when the level is 0
    (define edit-sequence-level 0)

    ;; Invoke the request-refresh callback, but only if we are not inside an
    ;; edit sequence and if another refresh is not pending.
    (define/public (refresh #:maybe-resize (maybe-resize #f) #:delay (refresh-delay #f))
      (if (and (real? refresh-delay) (positive? refresh-delay))
          (if auto-resize-to-fit?
              ;; This will also invoke a refresh...
              (send auto-resize-to-fit-timer start refresh-delay #t)
              ;; NO point in refreshing immediately as points won't be ready...
              (send redraw-timer start refresh-delay #t))
          (when (zero? edit-sequence-level)
            (when (box-cas! good-to-refresh? #t #f)
              (if (and auto-resize-to-fit? maybe-resize)
                  (resize-to-fit)
                  (request-refresh))))))

    ;; When #f, the tiles are not drawn, only the tracks.
    (define show-map-layer? #t)

    ;; When #t, a resize-to-fit is called after new data is added to the map
    ;; widget.  This flag is reset if the user moves or zooms the map.
    (define auto-resize-to-fit? #f)

    ;; When true a resize-to-fit was called inside an edit sequence and will
    ;; be executed at the end of the edit sequence.
    (define delayed-resize-to-fit? #f)

    ;; A flag to indicate whether a refresh can be called -- it is set by draw
    ;; and cleared by a `refresh` call, and it is used to avoid calling
    ;; another refresh before the draw operation happened, since draw always
    ;; draws the latest state anyway.
    (define good-to-refresh? (box #t))

    (define the-layers
      (list (new map-legend-layer% [name 'default-map-legend-layer] [admin this])))
    ;; Layers which want to receive mouse events
    (define the-mouse-event-layers
      null)
    (define the-zoom-level zoom)
    (define max-tile-num (expt 2 the-zoom-level))
    (define max-coord (* tile-size max-tile-num))
    (define origin-x 0)
    (define origin-y 0)

    (define/private (valid-tile-num? n) (and (>= n 0) (< n max-tile-num)))
    (define/public (get-max-coord) max-coord)
    (define/public (get-origin) (values origin-x origin-y))

    ;; Number of tiles in the width and height of the drawing area
    (define tw (add1 (exact-ceiling (/ width tile-size))))
    (define th (add1 (exact-ceiling (/ height tile-size))))

    ;; Copy the state of OTHER into this object instance.  This is used as a
    ;; helper method for map-snip% which needs to have a COPY method.
    (define/public (copy-from other)
      (set! the-zoom-level (send other zoom-level))
      (define-values (ox oy) (send other get-origin))
      (set! origin-x ox)
      (set! origin-y oy)
      (set! the-layers
            (for/list ([l (in-list (send other get-all-layers))])
              (define c (send l clone))
              c))
      (for ([l (in-list the-layers)])
        (send l set-admin this))
      (limit-origin width height)
      (refresh))

    ;; Adjust the map origin such that we don't have to draw past the map
    ;; edges at the current zoom level
    (define/private (limit-origin w h)
      (when (> (+ origin-x w) max-coord)
        (set! origin-x (- max-coord w)))
      (when (< origin-x 0)
        (set! origin-x 0))
      (when (> (+ origin-y h) max-coord)
        (set! origin-y (- max-coord h)))
      (when (< origin-y 0)
        (set! origin-y 0)))

    ;; Resize the map to W, H dimensions
    (define/public (resize w h)
      ;; the origin x and y are calculated off the old width and height!
      (set! origin-x (- (+ origin-x (/ width 2)) (/ w 2)))
      (set! origin-y (- (+ origin-y (/ height 2)) (/ h 2)))
      (limit-origin w h)
      (set! width w)
      (set! height h)
      (set! tw (add1 (exact-ceiling (/ width tile-size))))
      (set! th (add1 (exact-ceiling (/ height tile-size))))
      ;; Tell the bitmap cache how many tiles to keep in the cache
      (set-cache-threshold (* 10 tw th))

      ;; NOTE: do not call refresh here, we were informed by our "container"
      ;; (map-widget% or map-snip%) that we were resized, and they are
      ;; responsible for refreshing themselves if they consider it necessary.
      )

    (define last-mouse-x #f)
    (define last-mouse-y #f)

    ;; Determine which cursor to use for the specified mouse EVENT.  This is a
    ;; helper method for the map-snip% class.
    (define/public (adjust-cursor dc x y editorx editory event)
      (cond ((or (send event dragging?)
                 (send event button-down? 'left))
             hand-cursor)
            ((send event button-up? 'left) arrow-cursor)
            (#t #f)))

    (define/public (drag-map dx dy)
      (set! origin-x (- origin-x dx))
      (set! origin-y (- origin-y dy))
      (limit-origin width height)
      (refresh))

    ;; Handle a mouse event.  Return #t if the event was handled, #f
    ;; otherwise.
    (define/public (on-event dc x y editorx editory event)
      (cond ((send event button-down? 'left)
             (set! last-mouse-x (send event get-x))
             (set! last-mouse-y (send event get-y))
             ;; Return as "Not handled', let others maybe handle it
             #f)
            ((send event button-up? 'left)
             (set! last-mouse-x #f)
             (set! last-mouse-y #f)
             ;; Return as "Not handled', let others maybe handle it
             #f)
            ((send event dragging?)
             (let ((mouse-x (send event get-x))
                   (mouse-y (send event get-y)))
               (when (and last-mouse-x last-mouse-y)
                 (drag-map (- mouse-x last-mouse-x) (- mouse-y last-mouse-y)))
               (set! last-mouse-x mouse-x)
               (set! last-mouse-y mouse-y))
             (set! auto-resize-to-fit? #f)
             ;; Event was handled
             #t)
            (#t
             ;; Else pass it on to any layers that wish to handle mouse
             ;; events, and return true if they handled the event.
             ;; Automatically returns #f when no layers handle the event.
             (for/first ([l (in-list the-mouse-event-layers)])
               (send l on-mouse-event dc x y editorx editory event)))))

    ;; Handle a keyboard event.  Return #t if the event was handled, #f
    ;; otherwise.  Note that the wheel scroll with the mouse is received as
    ;; keyboard events.  Also, in an `editor-canvas%`, these events are
    ;; handled by the canvas for scrolling unless you call:
    ;;
    ;; (send canvas  wheel-step #f)
    ;;
    ;; We use wheel scroll to implement the zoom/unzoom functionality
    (define/public (on-char dc x y editorx editory event)
      ;; Implement map zoom-in and out using the mouse wheel.  Mouse wheel
      ;; zoom is handled by the key event
      (case (send event get-key-code)
        [(wheel-up up add)
         (when (< the-zoom-level (max-zoom-level))
           (zoom-level (add1 the-zoom-level)))
         #t]
        [(wheel-down down subtract)
         (when (> the-zoom-level (min-zoom-level))
           (zoom-level (sub1 the-zoom-level)))
         #t]
        [(#\c) (center-map)]
        [(#\f) (resize-to-fit)]
        (else #f)))

    ;; Clear the DC to a white color in the rectangle X,Y,WIDTH,HEIGHT -- this
    ;; is used when show-map-layer? is #f
    (define/private (clear-dc dc x y width height)
      (let ((old-brush (send dc get-brush))
            (old-pen (send dc get-pen)))
        (send dc set-pen
              (send the-pen-list find-or-create-pen "black" 0 'transparent))
        (send dc set-brush
              (send the-brush-list find-or-create-brush "white" 'solid))
        (send dc draw-rectangle 0 0 width height)
        (send dc set-pen old-pen)
        (send dc set-brush old-brush)))

    ;; Draw the map on the device context DC at position X, Y.  The width and
    ;; height of the map is stored in this object.  Note that the code must
    ;; not assume that the entire device context is covered by the map.
    (define/public (draw dc x y)
      (set-box! good-to-refresh? #t)
      (with-draw-context dc
        (lambda ()
          (with-clipping-rect dc x y width height
            (lambda ()
              (if show-map-layer?
                  (draw-map-tiles dc x y)
                  (clear-dc dc x y width height))
              (with-origin dc (- origin-x x) (- origin-y y)
                (lambda ()
                  (for ([layer (in-list the-layers)])
                    (send layer draw dc the-zoom-level))
                  (when debug?
                    (define bbox (get-bounding-box))
                    (when bbox
                      (draw-bounding-box dc bbox the-zoom-level))))))))))

    ;; Return the dimensions of the map
    (define/public (get-size)
      (values width height))

    ;; Timer to schedule a re-paint of the canvas when we have some missing
    ;; tiles -- hopefully the tiles will arrive by the time we get to re-paint
    (define redraw-timer
      (new one-shot-timer%
           [notify-callback
            (lambda ()
              ;; Not sure why this is needed, but timed redraws don't work
              ;; without it...
              (set-box! good-to-refresh? #t)
              (refresh))]))

    ;; Timer to schedule a resize-to-fit event when the bounding box of some
    ;; of the layers change.  Used by the point-cloud-layer% which supports
    ;; streaming in points.
    (define auto-resize-to-fit-timer
      (new one-shot-timer%
           [notify-callback
            (lambda ()
              (when auto-resize-to-fit?
                (set-box! good-to-refresh? #t)
                (resize-to-fit)))]))

    ;; Draw the map tiles on the device context DC at DX, DY.  Note that this
    ;; function does not assume that the map is drawn on the entire device
    ;; context.
    (define/private (draw-map-tiles dc dx dy)
      (send redraw-timer stop)

      ;; Use smoothing on high DPI displays, but not on low DPI ones (each
      ;; look better in the corresponding mode).
      (define old-smoothing (send dc get-smoothing))
      (if (> (get-display-backing-scale) 1.0)
          (send dc set-smoothing 'smoothed)
          (send dc set-smoothing 'unsmoothed))

      (let* ((request-redraw? #f)
             ;; Coordinates of the tile at canvas origin (need not be a valid
             ;; tile)
             (tile0-x (exact-floor (/ origin-x tile-size)))
             (tile0-y (exact-floor (/ origin-y tile-size)))

             ;; offset inside the tile where the canvas origin lives.
             (xofs (- origin-x (* tile0-x tile-size)))
             (yofs (- origin-y (* tile0-y tile-size))))

        (for* ((x (in-range 0 tw))
               (y (in-range 0 th)))
          (let ((tile-x (+ tile0-x x))
                (tile-y (+ tile0-y y)))
            (when (and (valid-tile-num? tile-x) (valid-tile-num? tile-y))
              (let ((bmp (or (get-tile-bitmap (tile the-zoom-level tile-x tile-y))
                             (begin (set! request-redraw? #t) empty-bmp))))
                (send dc draw-bitmap bmp
                      (+ dx (- (* x tile-size) xofs))
                      (+ dy (- (* y tile-size) yofs)))))))

        ;; NOTE: this is likely incorrect: we only start the refresh timer if
        ;; `allow-tile-download` is #t -- this is done to make the
        ;; trends-chart tests pass, but it is likely incorrect, as we need to
        ;; refresh even when tiles are retrieved from disk.
        (when (allow-tile-download)
          (cond
            (request-redraw?
             ;; We didn't get tiles we needed, maybe they are still fetched
             ;; from the database, request a redraw in a short amount of time.
             (send redraw-timer start 100 #t))
            ((> (get-download-backlog) 0)
             ;; So we have all the tiles we need, but more tiles are being
             ;; downloaded.  Request a redraw with a longer timeout, since
             ;; this will only update the tile backlog number.
             (send redraw-timer start 1000 #t))))

        (send dc set-smoothing old-smoothing)))

    ;; Set and get the current zoom level
    (public zoom-level)
    (define zoom-level
      (case-lambda
        [() the-zoom-level]
        [(zl)
         ;; Ensure the zoom level is in the valid range
         (when (> zl (max-zoom-level)) (set! zl (max-zoom-level)))
         (when (< zl (min-zoom-level)) (set! zl (min-zoom-level)))
         ;; Don't do anything unless the zoom level actually changes
         (unless (eq? zl the-zoom-level)
           (set! auto-resize-to-fit? #f)
           (let ((scale (expt 2 (- zl the-zoom-level))))
             (set! the-zoom-level zl)
             (set! max-tile-num (expt 2 the-zoom-level))
             (set! max-coord (* tile-size max-tile-num))
             ;; update the origin at the new zoom level (note that we scale
             ;; around the center of the view)
             (set! origin-x (- (* scale (+ origin-x (/ width 2))) (/ width 2)))
             (set! origin-y (- (* scale (+ origin-y (/ height 2))) (/ height 2))))
           (limit-origin width height)
           (for ([l (in-list the-layers)])
             (send l on-zoom-level-change zl))
           (refresh)
           (on-zoom-level-change the-zoom-level))]))

    (public show-map-layer)
    (define show-map-layer
      (case-lambda
        [() show-map-layer?]
        [(flag)
         (unless (equal? show-map-layer? flag)
           (set! show-map-layer? flag)
           (refresh))]))

    (define/public (on-zorder-changed)
      (set! the-layers (sort-layers-by-zorder the-layers))
      (refresh))

    (define/public (find-layer-by-name name)
      (for/first ([layer (in-list the-layers)]
                  #:when (equal? name (send layer get-name)))
        layer))

    (define/public (get-all-layers)
      the-layers)

    (define/public (add-layer layer)
      ;; Remove any previous layer by that name.
      ;; TODO: add a #:replace? option and error out if we try to add duplicates?
      (remove-layer (send layer get-name))
      (set! the-layers (sort-layers-by-zorder (cons layer the-layers)))
      (send layer set-admin this)
      (refresh))

    ;; Register a layer to receive mouse events -- such layers will have their
    ;; on-mouse-event method called on mouse events...
    (define/public (register-for-mouse-events layer-name)
      (define l (find-layer-by-name layer-name))
      (when l
        (unless (member l the-mouse-event-layers)
          (set! the-mouse-event-layers (cons l the-mouse-event-layers)))))

    (define/public (unregister-for-mouse-events layer-name)
      (define l (find-layer-by-name layer-name))
      (when l
        (set! the-mouse-event-layers (remove l the-mouse-event-layers))))

    (define/public (remove-layer layer-name)
      (if layer-name
          (let ([l (find-layer-by-name layer-name)])
            (when l
              (set! the-layers (remove l the-layers))
              (set! the-mouse-event-layers (remove l the-mouse-event-layers))
              (send l set-admin #f)))
          (begin
            (for ([l (in-list the-layers)])
              (send l set-admin #f))
            (set! the-layers '())
            (set! the-mouse-event-layers '())))
      (refresh))

    ;; Return the bounding box for all tracks in GROUP, or if GROUP is #f for
    ;; all tracks on the map.
    (define/private (get-bounding-box [group #f])
      (if group
          (let ([l (find-layer-by-name group)])
            (and l (send l get-bounding-box)))
          (for/fold ([outer #f])
                    ([layer (in-list the-layers)])
            (let ([bb (send layer get-bounding-box)])
              (if (and outer bb)
                  (bbox-merge outer bb)
                  (or outer bb))))))

    ;; Return the center position for all tracks in GROUP, or the center
    ;; position for all tracks when GROUP is #f
    (define/private (get-center [group #f])
      (let ([bbox (get-bounding-box group)])
        (if bbox
            (let ([cp/ndcs (bbox-center/ndcs bbox)])
              (values (* (npoint-x cp/ndcs) max-coord)
                      (* (npoint-y cp/ndcs) max-coord)))
            ;; For no particular reason, the center of the map, when no
            ;; bounding box is available is the middle of Swan River, Perth,
            ;; Western Australia
            (let ([p (lat-lon->npoint -31.974762 115.839303)])
              (values (* (npoint-x p) max-coord)
                      (* (npoint-y p) max-coord))))))

    ;; Move the map so that the tracks are centered in the middle.
    (define/public (center-map [group #f])
      (let-values (([cx cy] (get-center group)))
        (set! origin-x (- cx (/ width 2)))
        (set! origin-y (- cy (/ height 2))))
      (limit-origin width height)
      (refresh))

    ;; Resize (set the zoom level) and center the map so that all tracks in
    ;; GROUP are visible.  If GROUP is #f, resize and center the map such that
    ;; all tracks are visible.
    (define/public (resize-to-fit [group #f])
      ;; NOTE: we always force a refresh if we are resizing to the group,
      ;; since I am too lazy to record that the `delayed-resize-to-fit?`
      ;; refers to a group (currently it will resize to fit the entire track.
      (if (or (zero? edit-sequence-level) group)
          (let ((saved-flag auto-resize-to-fit?)
                (bbox (get-bounding-box group)))
            (when bbox
              (zoom-level (select-zoom-level bbox width height))
              ;; calling zoom-level will reset this flag (it needs to as
              ;; zoom-level is a public facing function), restore the previous
              ;; value here.
              (set! auto-resize-to-fit? saved-flag))
            (center-map group)
            (refresh))
          (set! delayed-resize-to-fit? #t)))

    ;; move the map such that POSITION is in the center
    (define/public (move-to position)
      (match-define (vector lat lon _ ...) position)
      (let* ([p (lat-lon->npoint lat lon)]
             [cx (* (npoint-x p) max-coord)]
             [cy (* (npoint-y p) max-coord)])
        (set! origin-x (- cx (/ width 2)))
        (set! origin-y (- cy (/ height 2))))
      (limit-origin width height)
      (refresh))

    ;; Write an image of the current map to FILE-NAME
    (define/public (export-image-to-file file-name)
      (let ((bmp (make-bitmap width height)))
        (draw (new bitmap-dc% [bitmap bmp]) 0 0)
        (send bmp save-file file-name 'png)))

    (public auto-resize-to-fit)
    (define auto-resize-to-fit
      (case-lambda
        [() auto-resize-to-fit?]
        [(flag)
         (set! auto-resize-to-fit? flag)
         flag]))

    ;; Start an edit sequence.  The map will not be refreshed while an edit
    ;; sequence is in progress, allowing the caller to make many map
    ;; modifications in one go without a refresh being queued in-between.  An
    ;; edit sequence is completed by calling `end-edit-sequence`.  Edit
    ;; sequence can be nested.
    (define/public (begin-edit-sequence)
      (set! edit-sequence-level (add1 edit-sequence-level)))

    ;; End an edit sequence started by `begin-edit-sequence`.
    (define/public (end-edit-sequence)
      (when (zero? edit-sequence-level)
        (error "map-impl%/end-edit-sequence: bad call"))
      (set! edit-sequence-level (sub1 edit-sequence-level))
      (when (zero? edit-sequence-level)
        (when delayed-resize-to-fit?
          (set! delayed-resize-to-fit? #f)
          (resize-to-fit))
        (refresh)))

    (if position (move-to position) (center-map))

    ))

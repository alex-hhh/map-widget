#lang racket/base

;; point-cloud.rkt -- render a point cloud (heat map) layer
;;
;; This file is part of map-widget
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

(require geoid
         racket/draw
         racket/class
         racket/math
         racket/match
         racket/format
         "map-util.rkt")

(provide point-cloud%)


;;........................................................ geoid-buffers ....

;; A geoid buffer holds a collection of geoids along with a "center" geoid
;; plus a rank which is the number of geoids inside the current one.  They are
;; used to collect geoids at a higher level from a lower level (geoids
;; resolution) geoids.
;;
;; The data representation is somewhat unusual, but it is designed for speed:
;; a single vector holds sequences of 3 values: the geoid, the center geoid
;; and the rank.  Traversal needs to take this into account, but the constants
;; below help with that.

(define gb-stride 3)
(define gb-geoid 0)
(define gb-center 1)
(define gb-rank 2)

;; The geoid structure itself.  `size` contains the number of elements (the
;; data occupied in the buffer is (* size gb-stride), data is the vector
;; containing the data, and may have more room than actual data, that is, (>=
;; (vector-length data) (* size gb-stride).
;;
;; Geoid buffers are updated in-place, in different threads, and a semaphore
;; controls access to this buffer.
(struct gbuffer
  ((size #:mutable)
   (data #:mutable)
   semaphore)
  #:transparent)

;; Create a new geoid buffer, capable of holding CAPACITY elements.  The geoid
;; buffer will be empty.
(define (make-gbuffer capacity)
  (gbuffer 0 (make-vector (* capacity gb-stride)) (make-semaphore 1)))

;; Return the number of elements that the geoid buffer can hold before having
;; to re-allocate the data buffer.  This is not the number of elements it
;; *actually* holds, that is given by the `size` slot.
(define (capacity gb)
  (inexact->exact (/ (vector-length (gbuffer-data gb)) gb-stride)))

;; Return the amount of free space (as number of entries) in the geoid buffer.
(define (free-space gb)
  (- (capacity gb) (gbuffer-size gb)))

;; Make room for at least `amount` entries in the gbuffer `gb`, reallocating a
;; new data buffer if necessary. The size of the buffer does not change.
(define (make-room! gb amount)
  (when (< (free-space gb) amount)
    (match-define (gbuffer size data _semaphore) gb)
    (define ndata (make-vector (* (+ (capacity gb) amount) gb-stride)))
    (vector-copy! ndata 0 data 0 (* size gb-stride))
    (set-gbuffer-data! gb ndata)))

;; Fill the geoid buffer GB with geoids, first updating them at LEVEL.  The
;; GEOIDS list must be sorted smallest to largest, and the GB will be cleared
;; and filled with the new sets of geoids.  Of several GEOIDS will upscale to
;; the same geoid at the specified LEVEL, the rank will increase to reflect
;; the number of grouped geoids.
(define (gbuffer-fill! gb geoids level)
  (call-with-semaphore
   (gbuffer-semaphore gb)
   (lambda ()
     (set-gbuffer-size! gb 0)              ; reset the buffer
     (when (pair? geoids)
       ;; .. and make sure we have enough room to put in all geoids (we might
       ;; actually reserve more space than we need, but we trade off memory usage
       ;; to avoid re-allocating the geoid buffer.
       (make-room! gb (length geoids))

       (define data (gbuffer-data gb))

       (define current-geoid (enclosing-geoid (car geoids) level))
       (define current-rank 1)
       (vector-set! data (+ 0 gb-geoid) current-geoid)
       (vector-set! data (+ 0 gb-center) (car geoids))
       (vector-set! data (+ 0 gb-rank) current-rank)
       (define-values (position _geoid rank)
         (for/fold ([position 0]
                    [current-geoid current-geoid]
                    [current-rank current-rank])
                   ([geoid (in-list (cdr geoids))])
           (define enclosing (enclosing-geoid geoid level))
           (if (= enclosing current-geoid)
               (values position current-geoid (add1 current-rank))
               (let ([next (+ position gb-stride)])
                 ;; Store previous rank
                 (vector-set! data (+ position gb-rank) current-rank)
                 (vector-set! data (+ next gb-geoid) enclosing)
                 (vector-set! data (+ next gb-center) geoid)
                 (vector-set! data (+ next gb-rank) 1)
                 (values next enclosing 1)))))
       ;; Store last rank update
       (vector-set! data (+ position gb-rank) rank)
       (set-gbuffer-size! gb (inexact->exact (add1 (/ position gb-stride))))))))

;; Re-group all geoids in the geoid buffer GB to be at the new LEVEL.  The
;; buffer is updated in place and ranks are accumulated if several geoids at
;; the lower level share the new geoids.
(define (gbuffer-upscale! gb level)
  (call-with-semaphore
   (gbuffer-semaphore gb)
   (lambda ()
     ;; Upscale the first geoid in the buffer and put it back.  Note that the
     ;; center and rank remain the same for the first one.
     (when (> (gbuffer-size gb) 0)
       (define data (gbuffer-data gb))
       (define enclosing (enclosing-geoid (vector-ref data (+ 0 gb-geoid)) level))
       (vector-set! data (+ 0 gb-geoid) enclosing)
       (define limit (* gb-stride (gbuffer-size gb)))
       ;; NOTE: center and rank for the first one remain the same.
       (define new-size
         (let loop ([enclosing enclosing]
                    [output-position 0]
                    [input-position gb-stride])
           (if (>= input-position limit)
               (inexact->exact (add1 (/ output-position gb-stride)))
               (let ([e (enclosing-geoid (vector-ref data (+ input-position gb-geoid)) level)])
                 (if (= enclosing e)
                     (let ([orank (vector-ref data (+ output-position gb-rank))]
                           [irank (vector-ref data (+ input-position gb-rank))])
                       (vector-set! data (+ output-position gb-rank) (+ orank irank))
                       (loop enclosing output-position (+ input-position gb-stride)))
                     (let ([center (vector-ref data (+ input-position gb-center))]
                           [rank (vector-ref data (+ input-position gb-rank))]
                           [output (+ output-position gb-stride)])
                       (vector-set! data (+ output gb-geoid) e)
                       (vector-set! data (+ output gb-center) center)
                       (vector-set! data (+ output gb-rank) rank)
                       (loop e output (+ input-position gb-stride))))))))
       (set-gbuffer-size! gb new-size)))))

;; Search for the geoid ELEMENT in the vector V between the START and END
;; positions, and return the insert location for it (or the actual element
;; position, if it is found).
(define (bsearch v start end element)
  (if (<= end start)
      start
      (let ([mid (exact-truncate (/ (+ start end) 2))])
        (define e (vector-ref v (+ (* mid gb-stride) gb-geoid)))
        (cond ((= e element) mid)
              ((< e element) (bsearch v (add1 mid) end element))
              ((> e element) (bsearch v start (sub1 mid) element))))))

;; Find the start end end positions in the geoid buffer GB where the geoids
;; from the other geoid buffer OTHER-GB would be inserted.
(define (find-insert-range gb other-gb)
  (match-define (gbuffer size data _semaphore) gb)
  (match-define (gbuffer osize odata _osemaphore) other-gb)
  (define start-geoid (vector-ref odata (+ 0 gb-geoid)))
  (define end-geoid (vector-ref odata (+ (* (sub1 osize) gb-stride) gb-geoid)))
  (define start (bsearch data 0 size start-geoid))
  (define end (bsearch data start size end-geoid))
  (values start end))

;; Increase the rank of the element at POSITION by RANK elements in the geoid
;; buffer GB.
(define (increase-rank! gb position rank)
  (define index (+ (* position gb-stride) gb-rank))
  (define data (gbuffer-data gb))
  (define orank (vector-ref data index))
  (vector-set! data index (+ orank rank)))

;; Amount of entries we add to the buffer if `insert-entry` needs to allocate
;; a new one -- we want to minimize the number of times we call
;; `make-room!` and we are sacrificing memory on that.
(define enlarge-step 1000)

;; Insert a new entry at BEFORE-POSITION, moving all entries one slot to the
;; right.  The entry is composed of a geoid, its center and a rank.  A new
;; data buffer may be allocated as a result of this.
(define (insert-entry! gb before-position geoid center rank)
  (when (< (free-space gb) 1)
    (make-room! gb enlarge-step))
  (match-define (gbuffer size data _semaphore) gb)
  (define mark (* before-position gb-stride))
  (vector-copy! data (+ mark gb-stride) data mark (* size gb-stride))
  (vector-set! data (+ mark gb-geoid) geoid)
  (vector-set! data (+ mark gb-center) center)
  (vector-set! data (+ mark gb-rank) rank)
  (set-gbuffer-size! gb (add1 size)))

;; Add the entries from the OTHER geoid buffer to the geoid buffer GB. The two
;; buffers must contain geoids at the same level.
(define (gbuffer-fold! gb other)
  (call-with-semaphore
   (gbuffer-semaphore gb)
   (lambda ()
     (call-with-semaphore
      (gbuffer-semaphore other)
      (lambda ()
        (when (> (gbuffer-size other) 0)
          ;; NOTE: we'll update `gb` several times, during the loop, so we don't
          ;; want to de-construct it!
          (match-define (gbuffer osize odata _osemaphore) other)
          (define-values (istart iend) (find-insert-range gb other))
          (cond
            ((equal? iend 0)
             ;; So, all our geoids come before the geoids in GB.  Just prepend them.
             (make-room! gb osize)
             (match-define (gbuffer size data _semaphore) gb)
             (vector-copy! data (* osize gb-stride) data 0 (* size gb-stride))
             (vector-copy! data 0 odata 0 (* osize gb-stride))
             (set-gbuffer-size! gb (+ size osize)))
            ((>= istart (gbuffer-size gb))
             ;; So all our geoids are after the geoids in `GB`.  Just append them.
             (make-room! gb osize)
             (match-define (gbuffer size data _semaphore) gb)
             (vector-copy! data (* size gb-stride) odata 0 (* osize gb-stride))
             (set-gbuffer-size! gb (+ size osize)))
            (#t
             (define read-limit (* osize gb-stride))
             (let loop ([insert-pos istart]
                        [read-pos 0])
               (if (= insert-pos (gbuffer-size gb))
                   ;; We reached the end of our buffer, all other entries are simply
                   ;; appended here.
                   (let ((remaining (/ (- read-limit read-pos) gb-stride)))
                     (make-room! gb remaining)
                     (match-define (gbuffer size data _semaphore) gb)
                     (vector-copy! data (* size gb-stride)
                                   odata (* (- osize remaining) gb-stride) (* osize gb-stride))
                     (set-gbuffer-size! gb (+ size remaining)))
                   (unless (= read-pos read-limit)
                     (define g1 (vector-ref odata (+ read-pos gb-geoid)))
                     (define g2 (vector-ref (gbuffer-data gb) (+ (* insert-pos gb-stride) gb-geoid)))
                     (cond
                       ((= g1 g2)
                        (define r (vector-ref odata (+ read-pos gb-rank)))
                        (increase-rank! gb insert-pos r)
                        (loop (add1 insert-pos) (+ read-pos gb-stride)))
                       ((> g1 g2)
                        (loop (add1 insert-pos) read-pos))
                       ((< g1 g2)
                        (define r (vector-ref odata (+ read-pos gb-rank)))
                        (define c (vector-ref odata (+ read-pos gb-center)))
                        (insert-entry! gb insert-pos g1 c r)
                        (loop (add1 insert-pos) (+ read-pos gb-stride)))))))))))))))


;; Extend the bounding box BB with geoids from the geoid buffer GB.  This
;; function is the slowest in this module, calculating an accurate bounding
;; box would account for 60-80% of the time it takes to process the geoids.
;; Instead, we randomly sample a small number of geoids and extend the
;; bounding box from those -- this seems to work well enough in practice.
(define (gbuffer-extend-bbox gb bb)
  (call-with-semaphore
   (gbuffer-semaphore gb)
   (lambda ()
     (match-define (gbuffer size data _semaphore) gb)
     (define sample-count (min size 10))
     (define samples (for/list ([x (in-range sample-count)]) (random size)))
     (define-values (min-lat min-lng max-lat max-lng)
       (for/fold ([min-lat (and bb (bbox-min-lat bb))]
                  [min-lng (and bb (bbox-min-lon bb))]
                  [max-lat (and bb (bbox-max-lat bb))]
                  [max-lng (and bb (bbox-max-lon bb))])
                 ([pos (in-list samples)])
         (define index (+ (* pos gb-stride) gb-geoid))
         (define-values (lat lng) (geoid->lat-lng (vector-ref data index)))
         (values
          (if min-lat (min min-lat lat) lat)
          (if min-lng (min min-lng lng) lng)
          (if max-lat (max max-lat lat) lat)
          (if max-lng (max max-lng lng) lng))))
     (if (and min-lat min-lng max-lat max-lng)
         (bbox max-lat max-lng min-lat min-lng)
         #f))))


;;............................................................... gcloud ....

;; A geoid cloud holds together a (large) collection of geoids and can
;; efficiently return them grouped at different levels.
(define gcloud%
  (class object%
    ;; MIN-LEVEL is the minimum geoid level we will request from the point
    ;; cloud.  This is the base level at which geoids are held and all other
    ;; higher levels will use `upscale-entries`.  It will not be possible to
    ;; retrieve a point cloud at a lower level than this.  The MAX-LEVEL is
    ;; similar for the maximum geoid level.
    (init-field [min-level 9] [max-level 30])
    (super-new)

    ;; Levels are from 0 to 30 (inclusive), of course we won't be using the
    ;; entire range here, but we also won't calculate point clouds for the
    ;; levels we don't use.
    ;;
    ;; TODO: this value should come from the geoid package.
    (define num-levels 31)

    ;; We re-calculate the bounding box when the size of the gbuffer at that
    ;; level has changed.  Since a gbuffer of the same size means no new
    ;; geoids have been added to it (maybe the rank has increased, but we
    ;; don't care about that here).
    (define bounding-box-gbuffer-size 0)

    ;; Bounding box for the geoid cloud (this is approximate, to make it fast)
    (define bounding-box #f)

    ;; Each time geoids are added to the point cloud via `add-geoids`, the
    ;; "generation" is incremented, allowing user code to determine if the
    ;; cloud has changed since draw buffers were constructed for it.
    (define generation 0)

    ;; The point cloud for each geoid level is a gbuffer, note that we start
    ;; by allocating a generous amount in the buffers to avoid re-allocation.
    (define entries (build-vector num-levels (lambda (_x) (make-gbuffer 10000))))

    ;; A buffer we use for the points `add-geoids` adds -- we keep it here to
    ;; avoid re-allocating it.
    (define working-gb (make-gbuffer 1000))

    ;; Add some geoids to the point cloud -- we assume that the POINTS list is
    ;; sorted.
    (define/public (add-geoids points)
      (when (pair? points)

        (gbuffer-fill! working-gb points min-level)

        (let* ([gb (vector-ref entries min-level)]
               [osize (gbuffer-size gb)])
          (gbuffer-fold! gb working-gb)
          (when (<= osize (gbuffer-size gb))
            (set! bounding-box (gbuffer-extend-bbox working-gb bounding-box))))

        (let loop ([level (add1 min-level)])
          (gbuffer-fold! (vector-ref entries level) working-gb)
          (when (< level max-level)
            (gbuffer-upscale! working-gb (add1 level))
            (loop (add1 level))))

        (set! generation (add1 generation))))

    ;; Return a point cloud at the specified LEVEL.
    ;;
    ;; If the stored point cloud at this level is outdated when compared to
    ;; the generation of the `min-level` a new point cloud is calculated by
    ;; `upscale-entries` from the highest up-to-date point cloud.
    (define/public (get-point-cloud level)
      (when (< level min-level)
        (error "get-point-cloud: requested level below minimum"))
      (when (> level max-level)
        (error "get-point-cloud: requested level above maximum"))
      (vector-ref entries level))

    ;; Return the current generation of the point cloud -- the generation can
    ;; be used by the calling code to determine if the point cloud has changed
    ;; and they need to update their own data structures.
    (define/public (get-generation)
      generation)

    ;; Return the bounding latitude/longitude for the entire point cloud.
    ;; Note that this is an approximate value, see notes on
    ;; `gbuffer-extend-bbox`
    (define/public (get-bounding-box)
      bounding-box)

    ))


;;.......................................................... draw-buffer ....

;; Draw buffers hold point cloud data at a specified zoom level, the data
;; being prepared for display (i.e. the coordinates are projected and scaled
;; for this zoom level and the scale and colors are pre-computed), the data is
;; held in a similar format as the geoid buffers: a single vector holds
;; sequences of elements.

(define tile-size 256)                  ; size of the map tiles, in pixels

(define db-stride 4)            ; number of items in the buffer for each point
(define db-x-offset 0)          ; offset for the x coordinate
(define db-y-offset 1)          ; offset for the y coordinate
(define db-size-offset 2)       ; offset for the point size
(define db-scale-offset 3)      ; offset for the rank percentage

;; Pre-calculate point positions and colors from `gb` (a gbuffer) and return a
;; vector with these positions and colors.  Such a draw buffer is specific to
;; the map zoom level.
;;
;; To make drawing more efficient, we pre-calculate the position and colors of
;; the geoid entries, since they will not change once all geoids are loaded.
;; The data is stored in a vector with consecutive entries (see `db-*`
;; definitions above).  This keeps things compact and fast to access for the
;; drawing code.
(define (make-draw-buffer gb
                          #:point-size point-size
                          #:zoom-level zoom-level)
  (call-with-semaphore
   (gbuffer-semaphore gb)
   (lambda ()
     (match-define (gbuffer size data _semaphore) gb)
     (define max-rank
       (for/fold ([max-rank 0])
                 ([rank-pos (in-range gb-rank (+ (* size gb-stride) gb-rank) gb-stride)])
         (max max-rank (vector-ref data rank-pos))))
     (define max-coord (* tile-size (expt 2 zoom-level)))
     (define buffer (make-vector (* db-stride size)))
     (for ([pos (in-range 0 (* size gb-stride) gb-stride)]
           [db-pos (in-range 0 (* size db-stride) db-stride)])
       (define rank (vector-ref data (+ pos gb-rank)))
       (define center (vector-ref data (+ pos (if (> rank 1) gb-geoid gb-center))))
       (match-define (npoint x y)
         (let-values ([(lat lon) (geoid->lat-lng center)])
           (lat-lon->npoint lat lon)))
       (define scale (/ rank max-rank))
       (define size (* point-size (+ 1 (* 1.5 scale))))
       (define half-size (/ size 2))
       (vector-set! buffer (+ db-pos db-x-offset) (- (* x max-coord) half-size))
       (vector-set! buffer (+ db-pos db-y-offset) (- (* y max-coord) half-size))
       (vector-set! buffer (+ db-pos db-size-offset) size)
       (vector-set! buffer (+ db-pos db-scale-offset) scale))
     buffer)))

;; Draw a buffer prepared by `make-draw-buffer` onto the device context DC.
;; The entries which are outside the draw buffer are manually culled so there
;; is no draw-call made for them.
(define (draw-buffer dc db color-map-brushes)
  (define ndrawn 0)
  (let-values (([ox oy] (send dc get-origin))
               ([width height] (send dc get-size)))
    (define x-min (- ox))
    (define y-min (- oy))
    (define x-max (+ (- ox) width))
    (define y-max (+ (- oy) height))
    (define max-color (sub1 (vector-length color-map-brushes)))
    (send dc set-pen (send the-pen-list find-or-create-pen "black" 0 'transparent))
    (define last-brush #f)
    (for ([pos (in-range 0 (vector-length db) db-stride)])
      (define scale (vector-ref db (+ pos db-scale-offset)))
      (define brush (exact-round (* scale max-color)))
      (unless (equal? brush last-brush)
        (set! last-brush brush)
        (send dc set-brush (vector-ref color-map-brushes brush)))
      (define x (vector-ref db (+ pos db-x-offset)))
      (define y (vector-ref db (+ pos db-y-offset)))
      (when (and (> x x-min) (< x x-max) (> y y-min) (< y y-max))
        (set! ndrawn (add1 ndrawn))
        (define size (vector-ref db (+ pos db-size-offset)))
        (send dc draw-ellipse x y size size))))
  ndrawn)

;; Default color map to use for geoids, from lightest to darkest (bigger
;; number of points)
(define default-color-map
  '((231 41 138)
    (226 22 111)
    (206 18 86)
    (180 8 77)
    (152 0 67)
    (128 0 47)
    (103 0 31)))

;; List a map zoom level to the geoid level and point size to be used for
;; drawing geoids at that map zoom level.  This is a manually tuned table
;; ensuring that the geoids look good at all zoom levels.
(define zoom-table
  '((18 11 7.0) ; NOTE: 10 would look better, but there is a huge cost for that
    (17 11 6.0) ; same here, 10 would look better
    (16 12 5.5)
    (15 13 6.5)
    (14 13 5.0)
    (13 14 5.0)
    (12 14 4.0)
    (11 15 4.0)
    (10 16 4.0)
    (9 17 4.0)
    (8 18 4.0)
    (7 19 4.5)
    (6 20 6.0)
    (5 21 6.0)
    (4 22 8.0)
    (3 23 9.0)
    (2 24 9.0)
    (1 25 9.0)))

;; Maintain the heat map for a specified map zoom level, this will only
;; maintain the actual draw buffer, the data is stored in the POINT-CLOUD
;; field which is shared by all zoom levels.
(define point-cloud-by-zoom-level%
  (class object%
    (init-field color-map
                zoom-level
                point-cloud
                [refresh-callback (lambda () (void))])
    (super-new)

    ;; The geoid level and point size to use for the current map zoom level.
    (define-values (geo-level point-size)
      (match (for/first ([entry (in-list zoom-table)]
                         #:when (equal? (car entry) zoom-level))
               entry)
        ((list zl gl ps) (values gl ps))
        (#f (values 20 2))))

    ;; Keep track of the draw buffer generation (compared to the point cloud
    ;; generation), so we know when to update the draw-buffer
    (define generation -1)
    (define db #f)                      ; the draw-buffer
    (define make-db-thread #f)
    (define the-brushes #f)             ; initialized by set-color-map.

    (define/public (set-color-map cm)
      (set! the-brushes
            (for/vector #:length (length cm)
                ([color (in-list cm)])
              (send the-brush-list find-or-create-brush color 'solid))))

    ;; Draw the heat map onto the device context.  The draw buffer is
    ;; refreshed if it is outdated.
    (define/public (draw dc)
      (define current-generation (send point-cloud get-generation))
      (unless (and db (= current-generation generation))
        ;; draw buffer needs to be re-created...
        (when (or (not make-db-thread) (thread-dead? make-db-thread))
          ;; ... and an update is not in progress
          (set! make-db-thread
                (thread
                 (lambda ()
                   (define entries (send point-cloud get-point-cloud geo-level))
                   (set! db
                         (make-draw-buffer entries
                                           #:point-size point-size
                                           #:zoom-level zoom-level))
                   (set! generation current-generation)
                   ;; Call refresh here to let the map widget know that we are
                   ;; ready for a redraw.
                   (refresh-callback))))))
      (define ndrawn (if db (draw-buffer dc db the-brushes) 0))
      #;(printf "zl = ~a, total ~a, drawn ~a~%" zoom-level (hash-count entries) ndrawn)
      ndrawn)

    (set-color-map color-map)

    ))

;; Convert ITEM from several possible color formats into a `color%` object.
(define (->color item)
  (cond ((is-a? item color%)
         item)
        ((list? item)
         (match-define (list r g b) item)
         (make-object color% r g b))
        ((string? item)
         (send the-color-database find-color ))
        (#t
         (error (format "Don't know how to make a color from ~a" item)))))

;; A point cloud layer on the map
(define point-cloud%
  (class object%
    (init-field
     color-map
     [refresh-callback (lambda () (void))])
    (super-new)

    (unless color-map
      (set! color-map (map ->color default-color-map)))

    (define gcloud
      ;; Use the minimum geo level from the zoom table as the minimum level
      ;; for the point cloud.  We won't encounter lower zoom levels...
      (let-values ([(min-level max-level)
                    (for/fold ([min-level 30]
                               [max-level 0])
                              ([e (in-list zoom-table)])
                      (define glevel (car (cdr e)))
                      (values (min min-level glevel) (max max-level glevel)))])
        (new gcloud% [min-level min-level] [max-level max-level])))

    ;; Heat maps for each zoom level -- created as needed
    (define by-zoom-level (make-hash))

    (define/public (add-points points #:format (fmt 'lat-lng))
      (define g
        (case fmt
          ((geoids) (sort points <))
          ((ordered-geoids) points)
          ((lat-lng)
           (define geoids
             (for/list ([p (in-list points)])
               (define-values (lat lng)
                 (cond ((list? p) (values (list-ref p 0) (list-ref p 1)))
                       ((vector? p) (values (vector-ref p 0) (vector-ref p 1)))
                       (#t (error "unknown point format"))))
               (lat-lng->geoid lat lng)))
           (sort geoids <))
          (else (error (format "point-cloud%/add-points: unknown format: ~a" fmt)))))
      (send gcloud add-geoids g))

    (define/private (pc-by-zoom-level zl)
      (define pc (hash-ref by-zoom-level zl #f))
      (unless pc
        (set! pc (new point-cloud-by-zoom-level%
                      [color-map color-map]
                      [zoom-level zl]
                      [point-cloud gcloud]
                      [refresh-callback refresh-callback]))
        (hash-set! by-zoom-level zl pc))
      pc)

    (define/public (get-bounding-box)
      (send gcloud get-bounding-box))

    (define/public (draw dc zoom-level)
      (send (pc-by-zoom-level zoom-level) draw dc))

    ;; Set a new color map for the heat map -- this is a list of colors from
    ;; "lowest density" to "highest density"
    (define/public (set-color-map cm)
      (set! color-map (map ->color cm))
      (for ([v (in-hash-values by-zoom-level)])
        (send v set-color-map color-map)))

    ))

#lang racket/base

;; point-cloud-tests.rkt -- tests for the point-cloud file
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

(require rackunit geoid racket/list)

;; TODO: test gbuffer-fill! with empty buffer

(require/expose "point-cloud.rkt"
                (gbuffer-size
                 gbuffer-data
                 gb-stride
                 gb-rank
                 gb-geoid
                 gb-center
                 make-gbuffer
                 insert-entry!
                 gbuffer-fill!
                 gbuffer-upscale!
                 gbuffer-fold!))

(define sample-size 7)
(define test-level 11)

(define sample-geoids
  (remove-duplicates
   (sort (for/list ([x (in-range sample-size)]) (random-geoid test-level)) <)))

(define (check-all-geoids-in-buffer gb geoids)
  ;; All the geoids should be counted, so we don't loose geoid counts as we
  ;; upscale
  (define all-ranks
    (for/sum ([pos (in-range (gbuffer-size gb))])
      (vector-ref (gbuffer-data gb) (+ (* gb-stride pos) gb-rank))))
  (check-equal? all-ranks (length geoids))
  ;; And all of them should be included
  (for ([geoid (in-list geoids)])
    (check-true
     (for/first ([pos (in-range (gbuffer-size gb))]
                 #:when
                 (let ([pg (vector-ref (gbuffer-data gb) (+ (* gb-stride pos) gb-geoid))])
                   (contains-geoid? pg geoid)))
       #t))))

(define gbuffer-test-suite
  (test-suite
   "gbuffer"
   (test-case "gbuffer-fill!"
     (define gb (make-gbuffer 0))          ; start with an empty gbuffer
     (gbuffer-fill! gb sample-geoids test-level)

     ;; Since we add geoids at the same level, and our geoids are unique, we
     ;; expect all of them to be in the buffer.
     (check-equal? (length sample-geoids) (gbuffer-size gb))
     ;; And to be there with a rank of 1
     (for ([(geoid pos) (in-indexed (in-list sample-geoids))])
       (define g
         (vector-ref (gbuffer-data gb) (+ (* gb-stride pos) gb-geoid)))
       (define r
         (vector-ref (gbuffer-data gb) (+ (* gb-stride pos) gb-rank)))
       (check-equal? g geoid)
       (check-equal? r 1))

     ;; Re-use the buffer to fill in with the same geoids, but each one
     ;; duplicated once.  We expect to only have `sample-size` geoids, but with a
     ;; rank of 2.
     (gbuffer-fill! gb (sort (append sample-geoids sample-geoids) <) test-level)
     (check-equal? (length sample-geoids) (gbuffer-size gb))
     ;; And to be there with a rank of 2
     (for ([(geoid pos) (in-indexed (in-list sample-geoids))])
       (define g
         (vector-ref (gbuffer-data gb) (+ (* gb-stride pos) gb-geoid)))
       (define r
         (vector-ref (gbuffer-data gb) (+ (* gb-stride pos) gb-rank)))
       (check-equal? g geoid)
       (check-equal? r 2))

     ;; Let's fill this at the max level
     (gbuffer-fill! gb sample-geoids 30)
     (check-all-geoids-in-buffer gb sample-geoids))

   (test-case "gbuffer-upscale! empty buffer"
     (define gb (make-gbuffer 0))
     (gbuffer-upscale! gb 10)
     (check-equal? (gbuffer-size gb) 0))

   (test-case "gbuffer-upscale! skip-levels"
     ;; Check that upscale works while upscaling from the base level to a
     ;; level higher up.
     (for ([level (in-range test-level 31)])
       (define gb (make-gbuffer 0))          ; start with an empty gbuffer
       (gbuffer-fill! gb sample-geoids test-level)
       (gbuffer-upscale! gb level)
       (check-all-geoids-in-buffer gb sample-geoids)))

   (test-case "gbuffer-upscale! one-by-one"
     ;; Check that upscale works when we scale from one level to the next.
     ;; Note that we also check upscaling to the same level.
     (define gb (make-gbuffer 0))          ; start with an empty gbuffer
     (gbuffer-fill! gb sample-geoids test-level)
     (for ([level (in-range test-level 31)])
       (gbuffer-upscale! gb level)
       (check-all-geoids-in-buffer gb sample-geoids)))

   (test-case "insert-entry"
     (define gb (make-gbuffer 0))

     ;; Insert an entry into the empty buffer
     (insert-entry! gb 0 1 2 3)
     (check-equal? (gbuffer-size gb) 1)
     (check-equal? (vector-ref (gbuffer-data gb) (+ 0 gb-geoid)) 1)
     (check-equal? (vector-ref (gbuffer-data gb) (+ 0 gb-center)) 2)
     (check-equal? (vector-ref (gbuffer-data gb) (+ 0 gb-rank)) 3)

     ;; Insert an entry at the front, pushing the first entry to the left
     (insert-entry! gb 0 4 5 6)
     (check-equal? (gbuffer-size gb) 2)
     (check-equal? (vector-ref (gbuffer-data gb) (+ 0 gb-geoid)) 4)
     (check-equal? (vector-ref (gbuffer-data gb) (+ 0 gb-center)) 5)
     (check-equal? (vector-ref (gbuffer-data gb) (+ 0 gb-rank)) 6)
     (check-equal? (vector-ref (gbuffer-data gb) (+ (* 1 gb-stride) gb-geoid)) 1)
     (check-equal? (vector-ref (gbuffer-data gb) (+ (* 1 gb-stride) gb-center)) 2)
     (check-equal? (vector-ref (gbuffer-data gb) (+ (* 1 gb-stride) gb-rank)) 3)

     ;; Insert an entry between the first and second entries
     (insert-entry! gb 1 7 8 9)
     (check-equal? (gbuffer-size gb) 3)
     (check-equal? (vector-ref (gbuffer-data gb) (+ 0 gb-geoid)) 4)
     (check-equal? (vector-ref (gbuffer-data gb) (+ 0 gb-center)) 5)
     (check-equal? (vector-ref (gbuffer-data gb) (+ 0 gb-rank)) 6)
     (check-equal? (vector-ref (gbuffer-data gb) (+ (* 1 gb-stride) gb-geoid)) 7)
     (check-equal? (vector-ref (gbuffer-data gb) (+ (* 1 gb-stride) gb-center)) 8)
     (check-equal? (vector-ref (gbuffer-data gb) (+ (* 1 gb-stride) gb-rank)) 9)
     (check-equal? (vector-ref (gbuffer-data gb) (+ (* 2 gb-stride) gb-geoid)) 1)
     (check-equal? (vector-ref (gbuffer-data gb) (+ (* 2 gb-stride) gb-center)) 2)
     (check-equal? (vector-ref (gbuffer-data gb) (+ (* 2 gb-stride) gb-rank)) 3)

     ;; Insert an entry at the end
     (insert-entry! gb 3 10 11 12)
     (check-equal? (gbuffer-size gb) 4)
     (check-equal? (vector-ref (gbuffer-data gb) (+ 0 gb-geoid)) 4)
     (check-equal? (vector-ref (gbuffer-data gb) (+ 0 gb-center)) 5)
     (check-equal? (vector-ref (gbuffer-data gb) (+ 0 gb-rank)) 6)
     (check-equal? (vector-ref (gbuffer-data gb) (+ (* 1 gb-stride) gb-geoid)) 7)
     (check-equal? (vector-ref (gbuffer-data gb) (+ (* 1 gb-stride) gb-center)) 8)
     (check-equal? (vector-ref (gbuffer-data gb) (+ (* 1 gb-stride) gb-rank)) 9)
     (check-equal? (vector-ref (gbuffer-data gb) (+ (* 2 gb-stride) gb-geoid)) 1)
     (check-equal? (vector-ref (gbuffer-data gb) (+ (* 2 gb-stride) gb-center)) 2)
     (check-equal? (vector-ref (gbuffer-data gb) (+ (* 2 gb-stride) gb-rank)) 3)
     (check-equal? (vector-ref (gbuffer-data gb) (+ (* 3 gb-stride) gb-geoid)) 10)
     (check-equal? (vector-ref (gbuffer-data gb) (+ (* 3 gb-stride) gb-center)) 11)
     (check-equal? (vector-ref (gbuffer-data gb) (+ (* 3 gb-stride) gb-rank)) 12))

   (test-case "gbuffer-fold! empty other"
     (define gb1 (make-gbuffer 0))
     (gbuffer-fill! gb1 sample-geoids test-level)
     (gbuffer-fold! gb1 (make-gbuffer 0))
     (check-equal? (gbuffer-size gb1) (length sample-geoids))
     (for ([(geoid pos) (in-indexed (in-list sample-geoids))])
       (define g
         (vector-ref (gbuffer-data gb1) (+ (* gb-stride pos) gb-geoid)))
       (define r
         (vector-ref (gbuffer-data gb1) (+ (* gb-stride pos) gb-rank)))
       (check-equal? g geoid)
       (check-equal? r 1)))

   (test-case "gbuffer-fold! empty buffer"
     (define gb1 (make-gbuffer 0))
     (gbuffer-fill! gb1 sample-geoids test-level)
     (define gb2 (make-gbuffer 0))
     (gbuffer-fold! gb2 gb1)
     (check-equal? (gbuffer-size gb1) (length sample-geoids))
     (for ([(geoid pos) (in-indexed (in-list sample-geoids))])
       (define g
         (vector-ref (gbuffer-data gb1) (+ (* gb-stride pos) gb-geoid)))
       (define r
         (vector-ref (gbuffer-data gb1) (+ (* gb-stride pos) gb-rank)))
       (check-equal? g geoid)
       (check-equal? r 1)))

   (test-case "gbuffer-fold! append"
     (define gb1 (make-gbuffer 0))
     (gbuffer-fill! gb1 (take sample-geoids 3) test-level)
     (define gb2 (make-gbuffer 0))
     (gbuffer-fill! gb2 (drop sample-geoids 3) test-level)
     (gbuffer-fold! gb1 gb2)

     (check-equal? (gbuffer-size gb1) (length sample-geoids))
     (for ([(geoid pos) (in-indexed (in-list sample-geoids))])
       (define g
         (vector-ref (gbuffer-data gb1) (+ (* gb-stride pos) gb-geoid)))
       (define r
         (vector-ref (gbuffer-data gb1) (+ (* gb-stride pos) gb-rank)))
       (check-equal? g geoid)
       (check-equal? r 1)))

   (test-case "gbuffer-fold! prepend"
     (define gb1 (make-gbuffer 0))
     (gbuffer-fill! gb1 (take sample-geoids 3) test-level)
     (define gb2 (make-gbuffer 0))
     (gbuffer-fill! gb2 (drop sample-geoids 3) test-level)
     (gbuffer-fold! gb2 gb1)

     (check-equal? (gbuffer-size gb2) (length sample-geoids))
     (for ([(geoid pos) (in-indexed (in-list sample-geoids))])
       (define g
         (vector-ref (gbuffer-data gb2) (+ (* gb-stride pos) gb-geoid)))
       (define r
         (vector-ref (gbuffer-data gb2) (+ (* gb-stride pos) gb-rank)))
       (check-equal? g geoid)
       (check-equal? r 1)))

   (test-case "gbuffer-fold! interleave"
     (define gb1 (make-gbuffer 0))
     (gbuffer-fill! gb1 (for/list ([(x index) (in-indexed (in-list sample-geoids))]
                                   #:when (odd? index))
                          x)
                    test-level)
     (define gb2 (make-gbuffer 0))
     (gbuffer-fill! gb2 (for/list ([(x index) (in-indexed (in-list sample-geoids))]
                                   #:when (even? index))
                          x)
                    test-level)
     (gbuffer-fold! gb2 gb1)

     (check-equal? (gbuffer-size gb2) (length sample-geoids))
     (for ([(geoid pos) (in-indexed (in-list sample-geoids))])
       (define g
         (vector-ref (gbuffer-data gb2) (+ (* gb-stride pos) gb-geoid)))
       (define r
         (vector-ref (gbuffer-data gb2) (+ (* gb-stride pos) gb-rank)))
       (check-equal? g geoid)
       (check-equal? r 1)))

   (test-case "gbuffer-fold! interleave 2"
     ;; Interleave the other way
     (define gb1 (make-gbuffer 0))
     (gbuffer-fill! gb1 (for/list ([(x index) (in-indexed (in-list sample-geoids))]
                                   #:when (odd? index))
                          x)
                    test-level)
     (define gb2 (make-gbuffer 0))
     (gbuffer-fill! gb2 (for/list ([(x index) (in-indexed (in-list sample-geoids))]
                                   #:when (even? index))
                          x)
                    test-level)
     (gbuffer-fold! gb1 gb2)

     (check-equal? (gbuffer-size gb1) (length sample-geoids))
     (for ([(geoid pos) (in-indexed (in-list sample-geoids))])
       (define g
         (vector-ref (gbuffer-data gb1) (+ (* gb-stride pos) gb-geoid)))
       (define r
         (vector-ref (gbuffer-data gb1) (+ (* gb-stride pos) gb-rank)))
       (check-equal? g geoid)
       (check-equal? r 1)))

   (test-case "gbuffer-fold! over itself"
     (define gb1 (make-gbuffer 0))
     (gbuffer-fill! gb1 sample-geoids test-level)
     (define gb2 (make-gbuffer 0))
     (gbuffer-fill! gb2 sample-geoids test-level)
     (gbuffer-fold! gb1 gb2)

     (check-equal? (gbuffer-size gb1) (length sample-geoids))
     (for ([(geoid pos) (in-indexed (in-list sample-geoids))])
       (define g
         (vector-ref (gbuffer-data gb1) (+ (* gb-stride pos) gb-geoid)))
       (define r
         (vector-ref (gbuffer-data gb1) (+ (* gb-stride pos) gb-rank)))
       (check-equal? g geoid)
       (check-equal? r 2)))

   ))

(module+ test
  (require al2-test-runner)
  (run-tests
   #:package "map-widget"
   #:results-file "test-results-point-cloud.xml"
   gbuffer-test-suite))

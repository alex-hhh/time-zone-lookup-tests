#lang racket

;; tzlookup2.rkt -- avoid an expensive point-in-polygon test if the point is
;; outside the bounding box.
;;
;; This file is part of Time Zone Lookup Tests -- https://github.com/alex-hhh/time-zone-lookup-tests
;; Copyright (c) 2019 Alex Harsányi <AlexHarsanyi@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

(require "al-profiler.rkt"
         json
         math/base)

;; A BOUNDING BOX contains the min/max coordinates that enclose a polygon.
(struct bbox (min-x min-y max-x max-y) #:transparent)

;; A POLYGON is defined as its bounding box and the list of points
(struct polygon (bbox points) #:transparent)

;; A SHAPE is defined as a polygon outline and a set of holes, which are also
;; polygons.
(struct shape (outline holes) #:transparent)

;; A FEATURE is a collection of shapes with a name (the timezone name in our
;; case).
(struct feature (name shapes) #:transparent)

;; Calculate the bounding box of a list of POINTS and return it as a `bbox`
;; struct.
(define (make-bbox points)
  (if (null? points)
      #f
      (let ([p0 (first points)])
        (let loop ([min-x (first p0)] [min-y (second p0)]
                   [max-x (first p0)] [max-y (second p0)]
                   [remaining (rest points)])
          (if (null? remaining)
              (bbox min-x min-y max-x max-y)
              (let ([p1 (first remaining)])
                (loop (min min-x (first p1)) (min min-y (second p1))
                      (max max-x (first p1)) (max max-y (second p1))
                      (rest remaining))))))))

;; Return #t if the point at LAT,LON is inside the bounding box BB
(define (inside-bbox? bb lat lon)
  (match-define (bbox min-x min-y max-x max-y) bb)
  (and (>= lon min-x) (<= lon max-x) (>= lat min-y) (<= lat max-y)))

;; Construct a polygon instance from a list of POINTS.  We calculate the
;; bounding box than store it and the points themselves in a POLYGON instance.
(define (make-polygon points)
  (polygon (make-bbox points) points))

;; Construct a SHAPE from a set GeoJSON Polygon.  The first item in the
;; GEOJSON-SHAPE list is a set of points for the outline, while the remaining
;; items are points for the holes.  We construct polygon instances for the
;; outline and the holes and construct a SHAPE instance with them.
(define (make-shape geojson-shape)
  (define outline (make-polygon (first geojson-shape)))
  (define holes (map make-polygon (rest geojson-shape)))
  (shape outline holes))

;; Construct a FEATURE from a GeoJSON feature node.  We extract the time zone
;; name and construct the appropriate SHAPE instances for it.
(define (make-feature geojson-feature)
  (define name
    (let ([properties (hash-ref geojson-feature 'properties)])
      (hash-ref properties 'tzid #f)))
  (define shapes
    (let ([geometry (hash-ref geojson-feature 'geometry (lambda () (hash)))])
      (let ([shapes (hash-ref geometry 'coordinates null)]
            [type (hash-ref geometry 'type #f)])
        (cond ((equal? type "Polygon")
               (list (make-shape shapes)))
              ((equal? type "MultiPolygon")
               ;; Must be in any of the shapes
               (map make-shape shapes))
              (#t
               (error (format "inside-feature? unsupported geometry type: ~a" type)))))))
  (feature name shapes))

;; Load a GeoJSON file from PATH.  This is just a wrapper for `read-json`, and
;; when VERBOSE? is #t, it will print out the amount of time it took to read
;; the file.  No other processing is done on the file.
(define/profile (load-geojson path #:verbose (verbose? #f))
  (define start (current-inexact-milliseconds))
  (when verbose?
    (printf "Loading GeoJSON from ~a ..." path)
    (flush-output))
  (define data (call-with-input-file path read-json))
  (when verbose?
    (define duration (- (current-inexact-milliseconds) start))
    (printf " done (~a seconds).~%" (~r (/ duration 1000.0) #:precision 2)))
  data)

(define/profile (prepare-features geojson)
  (for/list ([feature (hash-ref geojson 'features '())])
    (make-feature feature)))

;; Determine the angle seen from P0 between the points P1 and P2, or, more
;; formally, between the vectors P1 - P0 and P2 - P0. Returns a positive value
;; if P1, P2 are in a counter-clockwise direction w.r.t P0 and a negative
;; angle otherwise.
;;
;; See also: https://en.wikipedia.org/wiki/Subtended_angle
;;
(define/profile (subtended-angle p0 p1 p2)
  (match-define (list x0 y0) p0)
  (match-define (list x1 y1) p1)
  (match-define (list x2 y2) p2)

  (define s1x (- x1 x0))
  (define s1y (- y1 y0))
  (define s1len (sqrt (+ (* s1x s1x) (* s1y s1y))))
  (define s2x (- x2 x0))
  (define s2y (- y2 y0))
  (define s2len (sqrt (+ (* s2x s2x) (* s2y s2y))))
  
  (define dot-product (+ (* s1x s2x) (* s1y s2y)))
  
  (if (or (zero? dot-product) (zero? s1len) (zero? s1len))
      0
      (let ([angle (acos (min 1.0 (/ dot-product (* s1len s2len))))])
        (define cross-magnitude (- (* (- x1 x0) (- y2 y0))
                                   (* (- y1 y0) (- x2 x0))))
        (* angle (sgn cross-magnitude)))))


;; Calculate the winding number of POLYGON as seen from the point at LAT,LON.
;; A winding number greater than 1.0 will indicate that the point is inside
;; the polygon.
;;
;; Unlike the official winding number definition which is an integer, we
;; return a partial winding number, mostly to account for floating point
;; errors in our large and irregular polygons.  Also, the official winding
;; number definition has a sign, being negative if the polygon is traversed in
;; clockwise direction, but we don't care about that either, returning the
;; absolute value instead.
;;
;; https://en.wikipedia.org/wiki/Point_in_polygon and
;; https://en.wikipedia.org/wiki/Winding_number
;;
;; NOTE: this is the internal implementation, see `polygon-winding-number`
;; which does a bounding box check first and avoids this expensive
;; computation.  This exists as a separate function so its time can be
;; measured.
(define/profile (polygon-winding-number-internal poly lat lon)
  (let ([p0 (list lon lat)])
        (let loop ([winding-angle 0]
                   [remaining-points poly])
          (if (null? remaining-points)
              (abs (/ winding-angle (* 2 pi)))
              (let ([p1 (first remaining-points)]
                    [p2 (if (null? (rest remaining-points))
                            (first poly) ; cycle back to beginning, closing the polygon
                            (second remaining-points))])
                (define angle (subtended-angle p0 p1 p2))
                (loop (+ winding-angle angle) (rest remaining-points)))))))

(define/profile (polygon-winding-number poly lat lon)
  (if (inside-bbox? (polygon-bbox poly) lat lon)
      (polygon-winding-number-internal (polygon-points poly) lat lon)
      0))

;; Return the winding number of a "shape" with respect to the point at LAT,LON.
;;
;; In GeoJSON documents, a shape is an outline polygon and one or more "hole"
;; polygons.  If the point is inside the holes (the winding number of the hole
;; is close to 1), we return 0, otherwise we return the winding number of the
;; outline polygon, as returned by `polygon-winding-number`
;;
(define/profile (shape-winding-number p lat lon)
  ;; Must be inside the the outline (which is the first polygon) and not
  ;; inside any of the holes (which are the rest of the polygons).
  (let ([outline (shape-outline p)]
        [holes (shape-holes p)])
    (if (for/first ([h (in-list holes)] #:when (> (polygon-winding-number h lat lon) 0.9999)) #t)
        0
        (polygon-winding-number outline lat lon))))

;; Return the winding number of a GeoJSON feature with respect to the point at
;; LAT,LON.
;; 
(define/profile (feature-winding-number feature lat lon)
  (for/fold ([wn 0])
            ([shape (in-list (feature-shapes feature))])
    (max wn (shape-winding-number shape lat lon))))

;; Select a timezone from several CANDIDATES.  We return the candidate with
;; the biggest winding number, or resolve some close ties between the top two.
;;
;; See also the tz-lookup project for how some timezone conflicts are
;; resolved.  Since we have a completely different implementation, our
;; conflict sets are slightly different.
;;
;; https://github.com/darkskyapp/tz-lookup/blob/master/pack.js
;;
(define (select-candidate candidates)
  (define sorted (sort candidates > #:key second))
  (match sorted
    ;; NOTE candidates are sorted by their winding number, W1 >= W2!
    ((list (list n1 w1) (list n2 w2) other ...)
     (if (> (- w1 w2) 1e-2)
         n1                           ; w1 is definitely greater than w2
         (cond ((or (and (equal? n1 "Asia/Shanghai") (equal? n2 "Asia/Urumqi"))
                    (and (equal? n1 "Asia/Urumqi") (equal? n2 "Asia/Shanghai")))
                "Asia/Urumqi")          ; conflict zone
               (#t
                n1))))
    ((list (list n1 w1) other ...)
     n1)))

;; The timezone data JSON contains a 'features list, each feature being a time
;; zone definition
(define features
  (let ([tzdata (load-geojson "./data/combined.json" #:verbose #t)])
    (prepare-features tzdata)))

(define/profile (tz-lookup lat lon)
  (define candidates
    (for/list ([feature (in-list features)])
      (define wn (feature-winding-number feature lat lon))
      (list (feature-name feature) wn)))
  ;; Keep only candidates that have a large winding number
  (define filtered
    (for/list ([c (in-list candidates)] #:unless (< (second c) 0.1)) c))
  (cond ((null? filtered)
         #f)
        ((= (length filtered) 1)
         (first (first filtered)))
        (#t
         (printf "Multiple candidates for: ~a ~a: ~a~%" lat lon (sort filtered > #:key second))
         (select-candidate filtered))))

(module+ test
  (require rackunit)
  (profile-enable-all #t)
  (define test-data
    (call-with-input-file "./tz-test-cases.rktd" read))
  (define nitems (length test-data))
  (for ([(test-case num) (in-indexed (in-list test-data))])
    (match-define (list lat lon tzname) test-case)
    (printf "~a/~a checking ~a ~a ~a..." num nitems lat lon tzname)(flush-output)
    (check-equal? (tz-lookup lat lon) tzname)
    (printf "done.~%"))
  (profile-display))

#lang racket
;; tzpack10.rkt -- Compress the files
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
         math/flonum
         math/base
         racket/fasl
         file/gzip)

;; A BOUNDING BOX contains the min/max coordinates that enclose a polygon.
(struct bbox (min-x min-y max-x max-y) #:prefab)

;; A POLYGON is defined as its bounding box and the list of points
(struct polygon (bbox points) #:prefab)

;; A SHAPE is defined as a polygon outline and a set of holes, which are also
;; polygons.
(struct shape (outline holes) #:prefab)

;; A FEATURE is a collection of shapes with a name (the timezone name in our
;; case).
(struct feature (name shapes) #:prefab)

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

;; Construct a polygon instance from a list of POINTS.  We calculate the
;; bounding box than store it and the points themselves (converted to a
;; vector) in a POLYGON instance.
(define (make-polygon points)
  (define num-points (length points))
  (define data (make-vector (* 2 num-points)))
  (for ([(point index) (in-indexed (in-list points))])
    (match-define (list x y) point)
    (vector-set! data (* 2 index) (exact->inexact x))
    (vector-set! data (+ (* 2 index) 1) (exact->inexact y)))
  (polygon (make-bbox points) data))

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

(define (save-data data file-name)
  (define buffer (call-with-output-bytes (lambda (out) (s-exp->fasl data out))))
  (call-with-output-file file-name
    (lambda (out)
      (call-with-input-bytes
       buffer
       (lambda (in)
         (gzip-through-ports in out file-name (current-milliseconds)))))
    #:exists 'replace))

(module+ main
  ;; The timezone data JSON contains a 'features list, each feature being a
  ;; time zone definition
  (define features
    (let ([tzdata (load-geojson "./data/combined.json" #:verbose #t)])
      (prepare-features tzdata)))

  (make-directory* "./pack")

  ;; Save each feature in a separate file, the file name is constructed from
  ;; the feature name.
  (for ([feature (in-list features)])
    (define file-name (format "pack/~a.dat" (string-replace (feature-name feature) "/" "+")))
    ;; Save the data to a buffer, so we can compress it when writing it out.
    (save-data feature file-name))

  ;; Prepare and save an index mapping a bounding box to the feature name.
  (define index '())
  (for ([feature (in-list features)])
    (define name (feature-name feature))
    (for ([shape (in-list (feature-shapes feature))])
      (define outline (shape-outline shape))
      (set! index (cons (cons name (polygon-bbox outline)) index))))

  (save-data index "./pack/index.dat"))

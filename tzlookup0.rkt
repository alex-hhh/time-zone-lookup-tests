#lang racket
;; tzlookup0.rkt -- Basic version, runs the test suite, but fails, since the
;; code does not do anything.

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

(define (tz-lookup lat lon)
  #f)

(module+ test
  (require rackunit)
  (define test-data
    (call-with-input-file "./tz-test-cases.rktd" read))
  (define nitems (length test-data))
  (for ([(test-case num) (in-indexed (in-list test-data))])
    (match-define (list lat lon tzname) test-case)
    (printf "~a/~a checking ~a ~a ~a..." num nitems lat lon tzname)(flush-output)
    (check-equal? (tz-lookup lat lon) tzname)
    (printf "done.~%")))

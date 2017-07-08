#lang racket

(require "smallest-divisor.rkt")

(module+ test
  (require rackunit)

  (check-equal? 199 (smallest-divisor 199))
  (check-equal? 1999 (smallest-divisor 1999))
  (check-equal? 7 (smallest-divisor 19999)))
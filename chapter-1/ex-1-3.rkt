#lang racket

;; Определите процедуру, которая принимает в качестве аргументов три числа и
;; возвращает сумму квадратов двух больших из них.

(require "square.rkt")

(define (sum-of-max x y z)
  (if (and (<= x y) (<= x z))
           (+ (square y) (square z))
           (sum-of-max y z x)))

(module+ test
  (require rackunit)

  (test-case
   "sum-of-max returns a sum of two max numbers squared"

   (check-equal? 34 (sum-of-max 5 2 3))
   (check-equal? 13 (sum-of-max 1 2 3))
   (check-equal? 8 (sum-of-max 2 2 2))
   (check-equal? 18 (sum-of-max 3 3 1))
   (check-equal? 10 (sum-of-max -3 3 1))
   (check-equal? 10 (sum-of-max -3 -3 1))
   (check-equal? 10 (sum-of-max -3 3 -1))))

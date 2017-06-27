#lang racket

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(module+ test
  (require rackunit)

  (test-case
   "a-plus-abs-b returns a sum of a and b if b > 0 and subtraction otherwise"

   (check-equal? 4 (a-plus-abs-b 2 2))

   (check-equal? 4 (a-plus-abs-b 2 -2))))
#lang racket

(provide square)

(define (square x)
  (* x x))

(module+ test
  (require rackunit)

  (test-case
   "square returns a number squared"

   (check-equal? 4 (square 2))
   (check-equal? 9 (square 3))
   (check-equal? 16 (square (- 4)))))

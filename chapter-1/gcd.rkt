#lang racket

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(module+ test
  (require rackunit)

  (check-equal? 4 (gcd 4 4))
  (check-equal? 2 (gcd 206 40)))

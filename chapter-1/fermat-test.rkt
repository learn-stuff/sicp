#lang racket

(require "square.rkt")

(provide fast-prime?)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else false)))

(module+ test
  (require rackunit)

  (define (prime? n)
    (fast-prime? n 5))

  (test-case
   "prime? checks for prime numbers"
   (check-true (prime? 2))
   (check-true (prime? 3))
   (check-true (prime? 5))
   (check-true (prime? 1009))
   (check-false (prime? 4))
   (check-false (prime? 105))))

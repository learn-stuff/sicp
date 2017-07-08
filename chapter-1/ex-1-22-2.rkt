#lang racket

(require "smallest-divisor.rkt")

(define (runtime) (current-inexact-milliseconds))

(define (timed-prime-test n)
  (cond ((time (prime? n))
         (begin
           (display n)
           (display " *** ")
           (newline)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (search-for-primes from to)
  (cond ((<= from to)
         (if (odd? from)
             (begin (timed-prime-test from)
                    (search-for-primes (add1 from) to))
             (search-for-primes (add1 from) to)))))
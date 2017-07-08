#lang racket

(require "square.rkt")

(provide smallest-divisor)

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (next n)
  (if (= 2 n)
      3
      (+ n 2)))

(module+ test
  (require rackunit)

  (check-equal? 2 (smallest-divisor 4))
  (check-equal? 3 (smallest-divisor 21)))
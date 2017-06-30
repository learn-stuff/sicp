#lang racket

(require "square.rkt")

(define (fast-expt b n)
  (define (iter a b n)
    (cond ((= 0 n) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

(module+ test
  (require rackunit)

  (check-equal? (expt 1 0) (fast-expt 1 0))
  (check-equal? (expt 0 1) (fast-expt 0 1))
  (check-equal? (expt -2 3) (fast-expt -2 3))

  (check-equal? (expt 2 6) (fast-expt 2 6))
  (check-equal? (expt 2 4) (fast-expt 2 4))

  (check-equal? (expt 4 5) (fast-expt 4 5))
  (check-equal? (expt 4 25) (fast-expt 4 25)))
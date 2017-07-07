#lang racket

(require "square.rkt")

(define (sqrt x)
  (sqrt-iter 1.0 2.0 x))

(define (sqrt-iter guess old-guess x)
  (if (good-enough? guess old-guess)
      guess
      (sqrt-iter (improve guess x)
                 guess
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess old-guess)
  (< (/ (abs (- guess old-guess)) guess) 0.00001))


(module+ test
  (require rackunit)

  (check-= 2 (sqrt 4) 0.001)
  (check-= 3 (sqrt 9) 0.001)
  (check-= 3000000 (sqrt 9000000000000) 0.00001)
  (check-= 0.0003 (sqrt 0.00000009) 0.000000001)
  (check-= 0.00000000003 (sqrt 0.0000000000000000000009) 1e-12))
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
  (check-= 3e6 (sqrt 9e12) 1e-12)
  (check-= 3e-4 (sqrt 9e-8) 1e-9)
  (check-= 3e-11 (sqrt 9e-22) 1e-12))
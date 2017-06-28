#lang racket

(require "square.rkt")

(define (cubert x)
  (cubert-iter 1.0 2.0 x))

(define (cubert-iter guess old-guess x)
  (if (good-enough? guess old-guess)
      guess
      (cubert-iter (improve guess x)
                 guess
                 x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess old-guess)
  (< (/ (abs (- guess old-guess)) guess) 0.00001))


(module+ test
  (require rackunit)

  (check-= 2 (cubert 8) 0.001)

  (check-= 3 (cubert 27) 0.001))
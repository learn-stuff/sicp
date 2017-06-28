#lang racket

(require "square.rkt")

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))


(module+ test
  (require rackunit)

  (check-= 2 (sqrt 4) 0.001)

  (check-= 3 (sqrt 9) 0.001))
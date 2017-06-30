#lang racket

(define (cube x) (print 1) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

;; (/ (/ (/ (/ (/ 12.15 3) 3) 3) 3) 3) ;; 0.0499999

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

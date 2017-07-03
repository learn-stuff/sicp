#lang racket

(define (double num) (+ num num))

(define (halve num) (/ num 2))

(define (mult a b)
  (cond ((= b 0) 0)
        ((even? b) (mult (double a) (halve b)))
        (else (+ a (mult a (- b 1))))))

(module+ test
  (require rackunit)

  (check-equal? (mult 2 2) (* 2 2))
  (check-equal? (mult 5 8) (* 5 8))
  (check-equal? (mult -10 5) (* -10 5)))
#lang racket

(define (double num) (+ num num))

(define (halve num) (/ num 2))

(define (mult a b)
  (define (iter a b res)
    (cond ((= b 0) res)
        ((even? b) (iter (double a) (halve b) res))
        (else (iter a (- b 1) (+ res a)))))
  (iter a b 0))


(module+ test
  (require rackunit)

  (check-equal? (mult 2 2) (* 2 2))
  (check-equal? (mult 5 8) (* 5 8))
  (check-equal? (mult -10 5) (* -10 5)))
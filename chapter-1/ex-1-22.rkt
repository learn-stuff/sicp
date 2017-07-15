#lang racket

(require "smallest-divisor.rkt")

(define (runtime) (current-inexact-milliseconds))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (cond ((prime? n)
         (report-prime (- (runtime) start-time)))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (search-for-primes from to)
  (cond ((<= from to)
         (if (odd? from)
             (begin (timed-prime-test from)
                    (search-for-primes (add1 from) to))
             (search-for-primes (add1 from) to)))))


(module+ test
  (require rackunit)

  (test-case
   "prime? checks for prime numbers"
   (check-true (prime? 2))
   (check-true (prime? 3))
   (check-true (prime? 5))
   (check-true (prime? 1009))
   (check-false (prime? 4))
   (check-false (prime? 105))))

 (define primes-list (list 1009 1013 1019 10007 10009 10037 100003 100019 100043 1000003 1000033 1000037 1000352191 1000352201 1000352251))
 (for-each timed-prime-test primes-list)

;; 1009 *** 0.172119140625
;; 1013 *** 0.0029296875
;; 1019 *** 0.001953125
;; 10007 *** 0.007080078125
;; 10009 *** 0.007080078125
;; 10037 *** 0.0068359375
;; 100003 *** 0.02099609375
;; 100019 *** 0.02099609375
;; 100043 *** 0.02099609375
;; 1000003 *** 0.06494140625
;; 1000033 *** 0.06494140625
;; 1000037 *** 0.075927734375

;; (search-for-primes 1000 1020)
;; 1009 *** 0.0029296875
;; 1013 *** 0.003173828125
;; 1019 *** 0.003173828125

;; (* 0.0029296875 (sqrt 10)) ; 0.009264485332524549 ~ 0.007080078125

;; (search-for-primes 10000 10200)
;; 10007 *** 0.007080078125
;; 10009 *** 0.0078125
;; 10037 *** 0.0078125

;; (* 0.0078125 (sqrt 10)) ; 0.024705294220065465 ~ 0.024169921875

;; (search-for-primes 100000 100200)
;; 100003 *** 0.024169921875
;; 100019 *** 0.02392578125
;; 100043 *** 0.02294921875

;; (* 0.024169921875 (sqrt 10)) ; 0.07643200399332753 ~ 0.06201171875

;; (search-for-primes 1000000 1000200)
;; 1000003 *** 0.06201171875
;; 1000033 *** 0.06201171875
;; 1000037 *** 0.06201171875


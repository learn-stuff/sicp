#lang racket

(define (range n m)
  (cond
    ((= n m) (list n))
    (else (cons n (range ((if (< n m) + -) n 1) m)))))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (set! count (add1 count))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; Experiments

;; (run-with-counter 11)    ; '(4 . 55)
;; (run-with-counter 10)    ; '(4 . 41)
;; (run-with-counter 100)   ; '(292 . 15499)
;; (run-with-counter 200)   ; '(2435 . 229589)
;; (run-with-counter 1000)  ; '(801451 . 333082021)

(require plot)

(define count 0)

(define (run-with-counter amount)
  (set! count 0)
  (cons (count-change amount) count))

(define (run-only-counter amount)
  (set! count 0)
  (count-change amount)
  count)

(define (approximation n)
  (expt n 5))


(define upper-bound 400)
(define test-range (range 1 upper-bound))
(define xs test-range)
(define ys (map run-only-counter test-range))

(plot
  (points (map vector xs ys)))

(plot
  (function approximation 0 upper-bound))

(plot
 (mix
  (points (map vector xs ys))
  (function approximation)))

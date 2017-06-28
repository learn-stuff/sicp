#lang racket

;; Функция f определяется правилом: f(n) = n, если n < 3,
;; и f(n) = f(n-1)+f(n-2)+f(n-3), если n ≥ 3.
;; Напишите процедуру, вычисляющую f с помощью рекурсивного процесса.
;; Напишите процедуру, вычисляющую f с помощью итеративного процесса.

(define (f-recur n)
  (if (< n 3)
      n
      (+ (f-recur (- n 1))
         (f-recur (- n 2))
         (f-recur (- n 3)))))

(define (f-iter n)
  (define (iter a b c count)
    (cond ((= count 0) c)
          ((= count 1) b)
          ((= count 2) a)
          (else
           (iter (+ a b c) a b (sub1 count)))))
  (iter 2 1 0 n))


(module+ test
  (require rackunit)

  (test-case
   "f-recur"

   (check-equal? 0 (f-recur 0))
   (check-equal? 1 (f-recur 1))
   (check-equal? 2 (f-recur 2))
   (check-equal? 3 (f-recur 3))
   (check-equal? 6 (f-recur 4))
   (check-equal? 11 (f-recur 5)))

  (test-case
   "f-iter"

   (check-equal? 0 (f-iter 0))
   (check-equal? 1 (f-iter 1))
   (check-equal? 2 (f-iter 2))
   (check-equal? 3 (f-iter 3))
   (check-equal? 6 (f-iter 4))
   (check-equal? 11 (f-iter 5))))


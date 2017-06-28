#lang racket

;; Следующая процедура вычисляет математическую функцию, называемую функцией Аккермана.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; Рассмотрим следующие процедуры, где A — процедура, определенная выше:

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

;; Дайте краткие математические определения функций, вычисляемых процедурами f, g и h для
;; положительных целых значений n. Например, (k n) вычисляет 5n^2.

(define (f-simple n) (* 2 n))

(define (g-simple n)
  (if (= 0 n)
      0
      (expt 2 n)))

(define (h-simple n)
  (cond ((= 0 n) 0)
        ((= 1 n) 2)
        (else
         (expt 2 (h-simple (sub1 n))))))

(module+ test
  (require rackunit)

  (test-case
   "Ackermann test"

   (check-equal? 1024 (A 1 10))
   (check-equal? 65536 (A 2 4))
   (check-equal? 65536 (A 3 3)))

  (test-case
   "Ackermann formula"

   (check-equal? (expt 2 10) (A 1 10))
   (check-equal? (expt 2 (expt 2 (expt 2 2))) (A 2 4))
   (check-equal? (expt 2 (expt 2 (expt 2 2))) (A 3 3)))

  (test-case
   "f generalization 2n"

   (check-equal? (f 0) (f-simple 0))
   (check-equal? (f 1) (f-simple 1))
   (check-equal? (f 5) (f-simple 5)))

  (test-case
   "g generalization 0 or 2^n"

   (check-equal? (g 0) (g-simple 0))
   (check-equal? (g 1) (g-simple 1))
   (check-equal? (g 5) (g-simple 5)))

  (test-case
   "h generalization 0 or 2^(2^(2^(...2^2)))"

   (check-equal? (h 0) (h-simple 0))
   (check-equal? (h 1) (h-simple 1))
   (check-equal? (h 3) (h-simple 3))))
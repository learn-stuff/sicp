#lang racket

;; Лиза П. Хакер жалуется, что при написании expmod мы делаем много
;; лишней работы. В конце концов, говорит она, раз мы уже знаем, как
;; вычислять степени, можно просто написать
;; (define (expmod base exp m) (remainder (fast-expt base exp) m))
;; Права ли она? Стала бы эта процедура столь же хорошо работать
;; при проверке простых чисел? Объясните.

(require "square.rkt")

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else false)))


;; Prime implementation

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
  (fast-prime? n 5))

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

;; (define primes-list (list 1009 1013 1019 10007 10009 10037 100003 100019 100043 1000003 1000033 1000037 1000352191 1000352201 1000352251))
;; (for-each timed-prime-test primes-list)

;; Шаги редукции для случаев, когда степень больше 1, основаны на том,
;; что для любых целых чисел x, y и m мы можем найти остаток от деления
;; произведения x и y на m путем отдельного вычисления остатков x по модулю
;; m, y по модулю m, перемножения их, и взятия остатка по модулю m от
;; результата. Например, в случае, когда e четно, мы можем вычислить
;; остаток be/2 по модулю m, возвести его в квадрат и взять остаток по
;; модулю m. Такой метод полезен потому, что с его помощью мы можем
;; производить вычисления, не используя чисел, намного больших, чем m.
;; (Сравните с упражнением 1.25.)


;; 1009 *** 1.016845703125
;; 1013 *** 0.10888671875
;; 1019 *** 0.10986328125
;; 10007 *** 3.297119140625
;; 10009 *** 3.19091796875
;; 10037 *** 3.242919921875
;; 100003 *** 128.8779296875
;; 100019 *** 130.76513671875
;; 100043 *** 114.001953125
;; 1000003 *** 4557.48193359375
;; 1000033 *** 4291.157958984375
;; 1000037 *** 3795.56201171875
;; 1000352191 ...

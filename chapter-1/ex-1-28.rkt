#lang racket

;; Один из вариантов теста Ферма, который невозможно обмануть,
;; называется тест Миллера–Рабина (Miller-Rabin test) (Miller 1976; Rabin 1980). Он основан
;; на альтернативной формулировке Малой теоремы Ферма, которая состоит в том, что если n — простое число,
;; а a — произвольное положительное целое число, меньшее n, то a в n - 1-ой степени равняется 1 по
;; модулю n. Проверяя простоту числа n методом Миллера–Рабина, мы берем случайное число a < n и возводим
;; его в (n − 1)-ю степень по модулю n с помощью процедуры expmod. Однако когда в процедуре expmod мы
;; проводим возведение в квадрат, мы проверяем, не нашли ли мы «нетривиальный квадратный корень
;; из 1 по модулю n, то есть число, не равное 1 или n − 1, квадрат которого по модулю n равен 1.
;; Можно доказать, что если такой нетривиальный квадратный корень из 1 существует, то n не простое
;; число. Можно, кроме того, доказать, что если n — нечетное число, не являющееся простым, то по краийней
;; мере для половины чисел a < n вычисление a^n−1 с помощью такой процедуры обнаружит нетривиальный
;; квадратный корень из 1 по модулю n (вот почему тест Миллера–Рабина невозможно обмануть).
;; Модифицируйте процедуру expmod так, чтобы она сигнализировала обнаружение нетривиального
;; квадратного корня из 1, и используйте ее для реализации теста Миллера–Рабина с помощью процедуры,
;; аналогичной fermat-test. Проверьте свою процедуру на нескольких известных Вам простых и
;; составных числах. Подсказка: удобный способ заставить expmod подавать особый сигнал — заставить
;; ее возвращать 0.

(require "square.rkt")

(define carmichael-numbers (list 561 1105 1729 2465 2821 6601))

(define (rem-with-mr-check number m)
  (let ([rem (remainder (square number) m)])
    (if (and (not (= number (- m 1)))
             (not (= number 1))
             (= rem 1))
        0
        rem)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (rem-with-mr-check (expmod base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a (sub1 n) n) 1))
  (try-it (+ 1 (random (- n 2)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (prime? n)
  (fast-prime? n 5))


(module+ test
  (require rackunit)

  (test-case
   "rem-with-mr-check returns number squared modulo m"

   (check-equal? (rem-with-mr-check 13 5) 4))

  (test-case
   "returns modulo for a trivial numbers like m = number - 1"

   (check-equal? (rem-with-mr-check 2 3) 1))

  (test-case
   "returns 0 form non-trivial numbers with number squared modulo m equals 1"

   (check-equal? (rem-with-mr-check 10 3) 0)
   (check-equal? (rem-with-mr-check 3 8) 0))

  (test-case
   "prime? checks for prime numbers"
   
   (check-true (prime? 3))
   (check-true (prime? 5))
   (check-true (prime? 1009))
   (check-false (prime? 4))
   (check-false (prime? 105)))

  (test-case
   "prime? works for carmichael numbers"

   (check-false (prime? 561))
   (check-false (prime? 1105))
   (check-false (prime? 1729))
   (check-false (prime? 2465))
   (check-false (prime? 2821))
   (check-false (prime? 6601))))
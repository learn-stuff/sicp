#lang racket

;; Модифицируйте процедуру timed-prime-test из упражнения 1.22 так, чтобы она использовала
;; fast-prime? (метод Ферма) и проверьте каждое из 12 простых чисел, найденных в этом упражнении.
;; Исходя из того, что у теста Ферма порядок роста Θ(log n), то какого соотношения времени Вы бы
;; ожидали между проверкой на простоту поблизости от 1 000 000 и поблизости от 1000? Подтверждают
;; ли это Ваши данные? Можете ли Вы объяснить наблюдаемое несоответствие, если оно есть?

(require "fermat-test.rkt")

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
;; (define (log2 n) (/ (log n) (log 2)))
;; (for-each timed-prime-test primes-list)
;; (map log2 primes-list)


;; n              fast-prime        log2                 slow-prime
;; 1009           0.220947265625    9.978710459106358    0.172119140625
;; 1013           0.0078125         9.984418458801139    0.0029296875
;; 1019           0.008056640625    9.992938336165814    0.001953125
;; 10007          0.011962890625    13.28872191278264    0.007080078125
;; 10009          0.008056640625    13.28901022114509    0.007080078125
;; 10037          0.009033203125    13.29304050024473    0.0068359375
;; 100003         0.010009765625    16.60968375463883    0.02099609375
;; 100019         0.010009765625    16.60991456045723    0.02099609375
;; 100043         0.010986328125    16.61026069996546    0.02099609375
;; 1000003        0.011962890625    19.93157289740280    0.06494140625
;; 1000033        0.013916015625    19.93161617747499    0.06494140625
;; 1000037        0.011962890625    19.93162194805318    0.075927734375
;; 1000352191     0.01806640625     29.89786086874155    1.920166015625
;; 1000352201     0.016845703125    29.89786088316341    1.9599609375
;; 1000352251     0.018798828125    29.89786095527277    1.919921875

;; (/ 0.018798828125 0.008056640625) ~ (/ 29.89786095527277 9.978710459106358)
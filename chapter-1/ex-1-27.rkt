#lang racket

;; Покажите, что числа Кармайкла, перечисленные в сноске 47,
;; действительно «обманывают» тест Ферма: напишите процедуру,
;; которая берет целое число n и проверяет, правда ли an равняется
;; a по модулю n для всех a < n, и проверьте эту процедуру на этих
;; числах Кармайкла.

(require "fermat-test.rkt")

(define (range n m)
  (cond
    ((= n m) (list n))
    (else (cons n (range ((if (< n m) + -) n 1) m)))))

(define carmichael-numbers (list 561 1105 1729 2465 2821 6601))

(define (pass-fermat-test? n)
  (define (every-number? numbers)
    (if (null? numbers) #t
        (let ([test-number (car numbers)]
              [rest-numbers (cdr numbers)])
          (and
           (= test-number
              (expmod test-number n n))
           (every-number? rest-numbers)))))
  (every-number? (range 1 (sub1 n))))

(define (carmichael-numbers-work? numbers)
  (if (null? numbers) #t
      (let ([test-number (car numbers)]
            [rest-numbers (cdr numbers)])
        (and (pass-fermat-test? test-number)
             (carmichael-numbers-work? rest-numbers)))))


(module+ test
  (require rackunit)

  (check-true (carmichael-numbers-work? carmichael-numbers)))
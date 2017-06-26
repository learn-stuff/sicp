#lang racket

;; Переведите следующее выражение в префиксную форму

(define ex
  (/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
     (* 3 (- 6 2) (- 2 7))))

(module+ test
  (require rackunit)

  (test-case
   "square returns a number squared"
   (check-equal? (- 37/150) ex)))

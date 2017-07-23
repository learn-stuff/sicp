#lang racket

(define (cube n)
  (* n n n))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

(define (simpson-integral f a b n)
  (let* ([h (/ (- b a) n)]
         [sum-of-boundary-values (+ (f a) (f b))]
         [doubled-f (lambda (x) (* 2 (f x)))]
         [quadruple-f (lambda (x) (* 4 (f x)))]
         [next (lambda (x) (+ x (* 2 h)))])
    (* (/ h 3)
       (+ sum-of-boundary-values
          (sum doubled-f (+ a (* 2 h)) next (- b h))
          (sum quadruple-f (+ a h) next (- b (* 2 h)))))))

(module+ test
  (require rackunit)

  (test-case
   "cuber intergal"

   (check-= (integral cube 0 1 0.01) 1/4 1e-4)
   (check-= (integral cube 0 1 0.001) 1/4 1e-6))

  (test-case
   "cube intergal with simpson's method"

   (check-= (simpson-integral cube 0 1 100) 1/4 0.1)
   (check-= (simpson-integral cube 0 1 1000) 1/4 0.01)
   (check-= (simpson-integral cube 0 1 10000) 1/4 0.001)))
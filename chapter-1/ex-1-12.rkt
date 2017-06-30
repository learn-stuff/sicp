#lang racket

;; Приведенная ниже таблица называется треугольником Паскаля (Pascal’s triangle).
;;
;;           1
;;         1   1
;;       1   2   1
;;     1   3   3   1
;;   1   4   6   4   1
;;          ...
;;
;; Все числа по краям треугольника равны 1, а каждое число внутри треугольника
;; равно сумме двух чисел над ним. Напишите процедуру, вычисляющую элементы
;; треугольника Паскаля с помощью рекурсивного процесса.

(define (pascals-triangle row col)
  (if (or (= col 1) (= col row))
      1
      (+ (pascals-triangle (sub1 row) (sub1 col))
         (pascals-triangle (sub1 row) col))))

(module+ test
  (require rackunit)

  (check-equal? 1 (pascals-triangle 1 1))
  (check-equal? 1 (pascals-triangle 4 4))
  (check-equal? 2 (pascals-triangle 3 2))
  (check-equal? 4 (pascals-triangle 5 4)))

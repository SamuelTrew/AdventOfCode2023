#lang racket

(define inputs
   (map (lambda (line)
      (filter char-numeric? (string->list line)))
   (file->lines "inputs/1_1.txt"))
)

(define combinations
   (map (lambda (lst)
      (string->number (apply string (list (first lst) (last lst))))
   ) inputs)
)

(define day1 (foldr + 0 combinations))


(provide day1)
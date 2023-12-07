#lang racket
(require "days/1.rkt")
(require "days/2.rkt")

(define day (string->number (vector-ref (current-command-line-arguments) 0)))

(cond
   [(equal? 1 day) (displayln day1)]
   [(equal? 2 day) (displayln day2)]
   [(or (> day 25) (< day 1)) (displayln "Day has to be between 1 and 25")]
   [else "Day not implemented"]
)

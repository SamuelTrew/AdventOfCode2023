#lang typed/racket
(require/typed srfi/13 [string->list (-> String (Listof Char))])

(define lines : (Listof String)
   (map
      (lambda ([l : String]) : String
         (last (string-split l ": "))
      )
      (file->lines "inputs/2.txt")
   )
)

(define red 12)
(define green 13)
(define blue 14)

(define possible-games : (Listof Boolean)
   (map
      (lambda ([l : String]) : Boolean
         (foldl
            (lambda ([pair : String] [acc : Boolean]) : Boolean
               (define p (string-split pair " "))
               (define num (cast (string->number (first p)) Integer))
               (cond
                  [(equal? (last p) "red") (and acc (<= num red))]
                  [(equal? (last p) "green") (and acc (<= num green))]
                  [(equal? (last p) "blue") (and acc (<= num blue))]
                  [else #f]
               )
            )
            #t
            (string-split l #rx"(, |; )")
         )
      )
      lines
   )
)

(define part1 : Integer
   (foldl
      (lambda ([g : Boolean] [i : Integer] [acc : Integer]) : Integer
         (cond
            [(equal? g #t) (+ i acc)]
            [else acc]
         )
      )
      0
      possible-games
      (range 1 (+ 1 (length lines)))
   )
)

; Part 2
(define tuples : (Listof (List Integer Integer Integer))
   (map
      (lambda ([l : String]) : (List Integer Integer Integer)
         (foldl
            (lambda ([pair : String] [acc : (List Integer Integer Integer)]) : (List Integer Integer Integer)
               (define p (string-split pair " "))
               (define num (cast (string->number (first p)) Integer))
               (cond
                  [(equal? (last p) "red")
                     (list (max (first acc) num) (second acc) (last acc))
                  ]
                  [(equal? (last p) "green")
                     (list (first acc) (max (second acc) num) (last acc))
                  ]
                  [(equal? (last p) "blue")
                     (list (first acc) (second acc) (max (last acc) num))
                  ]
                  [else acc]
               )
            )
            (list 0 0 0)
            (string-split l #rx"(, |; )")
         )
      )
      lines
   )
)

(define part2 : Integer
   (apply +
      (map
         (lambda ([l : (List Integer Integer Integer)]) : Integer
            (apply * l)
         )
         tuples
      )
   )
)

(define day2 (list part1 part2))


(provide day2)
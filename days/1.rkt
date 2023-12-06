#lang typed/racket
; (require srfi/13)
(require/typed srfi/13 [string->list (-> String (Listof Char))])

(define lines : (Listof String)
   (file->lines "inputs/1.txt")
)

(define (input-filter [lines : (Listof String)]) : (Listof (Listof Char))
   (map
      (lambda ([line : String]) : (Listof Char)
         (filter char-numeric? (string->list line))
      )
      lines
   )
)

(define (combine-filtered [inputs : (Listof String)]) : (Listof Integer)
   (map
      (lambda ([lst : (Listof Char)]) : Integer
         (cast (string->number (list->string (list (first lst) (last lst)))) Integer)
      )
      (input-filter inputs)
   )
)

(define part1 : (Listof Integer) (combine-filtered lines))

; Part 2

(define replacements : (Listof (List String String))
   (list
      '("one" "o1e")
      '("two" "t2o")
      '("three" "t3e")
      '("four" "4")
      '("five" "5e")
      '("six" "6")
      '("seven" "7")
      '("eight" "e8t")
      '("nine" "9e")
   )
)

(define replaced : (Listof String)
   (map
      (lambda ([line : String]): String
         (foldl
            (lambda ([val : (List String String)] [acc : String]) : String
               (string-replace acc (first val) (last val))
            )
            line
            replacements
         )
      )
      lines
   )
)

(define part2 : (Listof Integer) (combine-filtered replaced))

(define day1 (list (foldr + 0 part1) (foldr + 0 part2)))


(provide day1)
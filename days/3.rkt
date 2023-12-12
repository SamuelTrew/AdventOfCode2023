#lang typed/racket
(require/typed srfi/13 [string->list (-> String (Listof Char))])

(define grid : (Listof (Listof Char))
   (map
      (lambda ([line : String]) : (Listof Char)
         (string->list line)
      )
      (file->lines "./inputs/3.txt")
   )
)

(define height : Integer (length grid))
(define width : Integer (length (first grid)))

(define (check-char-bounds [y : Integer] [x : Integer])
   (cond
      [(< y 0) #f]
      [(< x 0) #f]
      [(> y (- height 1)) #f]
      [(> x (- width 1)) #f]
      [else #t]
   )
)

(define (get-val [y : Integer] [x : Integer]) : Char
   (list-ref (list-ref grid y) x)
)

(define (is-gear [y : Integer] [x : Integer]) : Boolean
   (cond
      [(not (check-char-bounds y x)) #f]
      [else
         (cond
            [(char-numeric? (get-val y x)) #f]
            [(equal? #\. (get-val y x)) #f]
            [else #t]
         )
      ]
   )
)

(define (num-size [y : Integer] [x : Integer]) : Integer
   (cond
      [(and (check-char-bounds y (+ x 1)) (char-numeric? (get-val y (+ x 1))))
         (+ 2
            (cond
               [(and (check-char-bounds y (+ x 2)) (char-numeric? (get-val y (+ x 2)))) 1]
               [else 0]
            )
         )
      ]
      [else 1]
   )
)

(define (list-bounds [y : Integer] [x : Integer]) : (Listof (List Integer Integer))
   (foldl
      (lambda ([i : Integer] [acc : (Listof (List Integer Integer))])
         (append acc (list (list y (+ i x)) (list (- y 1) (+ i x)) (list (+ y 1) (+ i x))))
      )
      '()
      (range -1 (+ 1 (num-size y x)))
   )
)


(define (sublist [list : (Listof Char)] [start : Integer] [number : Integer]) : (Listof Char)
   (cond
      ((> start 0) (sublist (cdr list) (- start 1) number))
      ((> number 0) (cons (car list)
         (sublist (cdr list) 0 (- number 1))))
      (else '())
   )
)

(define (remove-leading-zeros [lst : (Listof Char)]) : (Listof Char)
   (cond
      [(equal? 1 (length lst)) lst]
      [(equal? (first lst) #\0) (remove-leading-zeros (list-tail lst 1))]
      [else lst]
   )
)

; Return num if next to gear, otherwise 0
(define (get-num [y : Integer] [x : Integer])
   (foldl
      (lambda ([coord : (List Integer Integer)]  [acc : Integer])
         (cond
            [(is-gear (first coord) (last coord))
               (cast
                  (string->number
                     (list->string
                        (remove-leading-zeros (sublist (list-ref grid y) x (num-size y x)))
                     )
                  )
                  Integer
               )
            ]
            [else acc]
         )
      )
      0
      (list-bounds y x)
   )
)

(define reductions : (Listof (Listof Integer))
   (map
      (lambda ([i : Integer])
         (map
            (lambda ([j : Integer])
               (cond
                  [(char-numeric? (get-val i j)) (get-num i j)]
                  [else 0]
               )
            )
            (range 0 width)
         )
      )
      (range 0 height)
   )
)

(define final-list : (Listof (Listof Integer))
   (map
      (lambda ([i : Integer])
         (map
            (lambda ([j : Integer])
               (define curr (list-ref (list-ref reductions i) j))
               (cond
                  [(<= j 0) curr]
                  [else (cond
                     [(>= (list-ref (list-ref reductions i) (- j 1)) curr) 0]
                     [else curr]
                  )]
               )
            )
            (range 0 width)
         )
      )
      (range 0 height)
   )
)

(define part1 : Integer (apply + (map (lambda ([l : (Listof Integer)]) (apply + l)) final-list)))

; Part 2

(define part2 : Integer 0)

(define day3 (list part1 part2))


(provide day3)
#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Fall 2022
;;
;; Parser
;;
;; 
;; 
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide parse)

(require racket/trace)

;; The main parser
;; 
(define parse
(lambda (str)
    (first (L (string->list str)))
))

;; Check if character is symbolic
(define char-symbolic?
(lambda (char) (and (not (char-whitespace? char))
(not (eq? char #\())
(not (eq? char #\))))))

;; Parse list
(define L
(lambda (input)
    (cond
    [(null? input) (cons null null)]
    [(char-whitespace? (first input)) (L (rest input))]
    [(or
        (D? (first input))
        (A? (first input))
        (eq? #\( (first input)))
    (let*
      ( (e-rest (E input))
        (l-rest (L (rest e-rest))))
      (cons (cons (first e-rest) (first l-rest)) (rest l-rest)))
    ]
    [else (cons null (rest input))]
    )
))
;(trace L)

;; Parse expression
(define E
(lambda (input)
    (cond
    [(D? (first input))
      (let*
        ( (d-rest (D input))
          (n-rest (N (rest d-rest) (first d-rest))))
        n-rest)
    ]
    [(A? (first input))
      (let*
        ( (a-rest (A input))
          (s-rest (S (rest a-rest) (first a-rest))))
        s-rest)
    ]        
    [(eq? #\( (first input)) (L (rest input))]
    [else (error "Syntax error. Rest: " (list->string input))]
    )
))
;(trace E)

;; Parse number
(define N
(lambda (input carry)
    (cond [(null? input) (cons carry input)]
    [(D? (first input)) (let*
        ( (d-rest (D input))
          (new-carry (+ (* carry 10) (first d-rest)))
          (n-rest (N (rest d-rest) new-carry)))
        n-rest
    )]
    [else (cons carry input)]
  )
))
;(trace N)

;; Parse string
(define S
(lambda (input carry)
    (cond
    [(or (null? input) (not (A? (first input)))) (cons (string->symbol carry) input)]      
    [else (let*
        ( (a-rest (A input))
          (new-carry (string-append carry (first a-rest)))
          (s-rest (S (rest a-rest) new-carry)))
        s-rest
    )]
  )
))
;(trace S)

;; Parse digit
(define D
(lambda (input)
    (cond
    [(D? (first input)) (cons (char->number (first input))
      (rest input))]           
    [else (error (string-append "Not a digit:"
      (list->string input)))]
    )
))
;(trace D)

;; Parse symbolic character
(define A
(lambda (input)
    (cond
    [(A? (first input)) (cons (string (first input))
      (rest input))]           
    [else (error (string-append "Not symbolic:"
      (list->string input)))]
    )
))
;(trace A)

;; Char to Number
;; Converts a char containing a digit to the integer value.  For example
;; (char->number #\4) => 4.
(define char->number
(lambda (char)
    (- (char->integer char)
       (char->integer #\0))
))

;; predicate for symbolic values.
(define A? char-symbolic?)

;; Predicate for decimal values.
(define D? char-numeric?)

#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Fall 2022
;;
;; Interpreter
;;
;; 
;; 
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/trace)

(provide
    lookup
    evaluate
    special-form?
    evaluate-special-form
)

; closure interface
(define closure
    (lambda (vars body env) (mcons 'closure (mcons env (mcons vars body)))))
(define closure?
    (lambda (clos) (and (mpair? clos) (eq? (mcar clos) 'closure))))
(define closure-env
    (lambda (clos) (mcar (mcdr clos))))
(define closure-vars
    (lambda (clos) (mcar (mcdr (mcdr clos)))))
(define closure-body
    (lambda (clos) (mcdr (mcdr (mcdr clos)))))
(define set-closure-env!
    (lambda (clos new-env) (set-mcar! (mcdr clos) new-env)))

; look up the representation of a symbol and get its value
(define lookup-rec
(lambda (symbol environment)
    (cond
        [(empty? environment)
            (error "Symbol not in environment.") ; no such symbol
        ]
        [(equal? symbol (car (car environment)))
            (car (cdr (car environment))) ; found the symbol
        ]
        [else (lookup-rec symbol
            (cdr environment)) ; keep recursing
        ]
    )
))

; look up symbol in environment
(define lookup
(lambda (symbol environment)
    (cond
        [(symbol? symbol) (lookup-rec symbol environment)]
        [else (error "Argument is not a symbol.")]
    )
))

; apply a closure to a list of arguments
(define apply-closure
(lambda (closure args)
    (evaluate (closure-body closure)
    (append (map list (closure-vars closure) args) (closure-env closure)))
))

; apply the first element to the rest
; if the first element is not a procedure, then error
(define apply-function
(lambda (closure values)
    (cond
        [(procedure? closure) (apply closure values)]
        [(closure? closure) (apply-closure closure values)]
        [else (error "First element of list is not a procedure.")]
    )
))

; evaluate an expression
(define evaluate
(lambda (expression environment)
    (cond
        [(number? expression) expression]
        [(symbol? expression) (lookup expression environment)]
        [(special-form? expression) (evaluate-special-form expression environment)]
        [(list? expression)
            (apply-function
                (evaluate (car expression) environment)
                (map (lambda (x) (evaluate x environment)) (cdr expression))
            )
        ]
        [else (error "Expression is unknown.")]
    )
))

; check if expression is a special form
(define special-form?
(lambda (expression)
    (and
        [list? expression]
        (or
            [eq? (car expression) 'if]
            [eq? (car expression) 'cond]
            [eq? (car expression) 'let]
            [eq? (car expression) 'lambda]
            [eq? (car expression) 'letrec]
        )
    )
))

; evaluate each statement of cond form
(define evaluate-cond-rec
(lambda (expression environment)
    (cond
        [(empty? expression) null]
        [(evaluate (car (car expression)) environment) ; met condition
            (evaluate (car (cdr (car expression))) environment)]
        [else (evaluate-cond-rec (cdr expression) environment)]
    )
))

; evaluate a symbol-value list and append it to an environment
(define let-eval
(lambda (expression environment)
    (append
        (map (lambda (x)
            (list (car x) (evaluate (car (cdr x)) environment)))
            expression)
        environment
    )
))

; evaluate a letrec special form
(define evaluate-letrec
(lambda (expression environment)
    (let* [(old-env environment)
            (mini-env (map (lambda (x) ; env containing let vars
                (list (car x) (evaluate (car (cdr x)) old-env)))
                expression))
           (new-env (append mini-env old-env))
           ]
           (modify-env mini-env new-env)
    )
))

; set the environment for every closure defined in mini-env to new-env
(define modify-env
(lambda (mini-env new-env)
    (cond
        [(empty? mini-env) new-env]
        [else
            (cond [(closure? (car (cdr (car mini-env))))
                (set-closure-env! (car (cdr (car mini-env))) new-env)])
            (modify-env (cdr mini-env) new-env)
        ]
    )
))

; evaluate a special form
(define evaluate-special-form
(lambda (expression environment)
    (cond
        [(eq? (car expression) 'if)
            (cond ; check if T or F
                [(evaluate (cadr expression) environment)
                    (evaluate (caddr expression) environment)
                ]
                [else (evaluate (cadddr expression) environment)]
            )
        ]
        [(eq? (car expression) 'cond)
            (evaluate-cond-rec (cdr expression) environment)
        ]
        [(eq? (car expression) 'let)
            (evaluate (caddr expression) (let-eval (cadr expression) environment))
        ]
        [(eq? (car expression) 'lambda)
            (closure (cadr expression) (caddr expression) environment)
        ]
        [(eq? (car expression) 'lambda)
            (closure (cadr expression) (caddr expression) environment)
        ]
        [(eq? (car expression) 'letrec)
            (evaluate (caddr expression) (evaluate-letrec (cadr expression) environment))
        ]
        [else (error "Special form is unknown.")]
    )
))

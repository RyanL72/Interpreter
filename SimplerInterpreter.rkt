#lang racket
; Get rid of assoc
; Get rid of some of the lets
; Find where certain functions call other functions and replace with new names 

;===============================================================
; Simple Interpreter
;
; CSDS 345
; 
; This interpreter reads a file containing a simple Java/C-ish language,
; parses it using the provided parser (simpleParser.rkt and lex.rkt),
; and then evaluates the resulting syntax tree.
;
; The interpreter supports variable declarations (with and without assignment),
; assignment expressions (including nested assignments), arithmetic and boolean
; operations, comparisons, if statements, while loops, and return statements.
;
; The interpreter is written in a pure, state‐passing style. Every expression
; evaluation returns a pair: (cons value updated-state). Statements thread the
; state similarly.
;===============================================================

(require "simpleParser.rkt")
(require "lex.rkt")


; MANAGING STATES

; Create an empty state (an association list)

(define (make-empty-state)
  '())

; Lookup a variable in the state. Error if not found or unassigned.

(define state_lookup
  (lambda (state var)
  (let ([binding (assoc var state)])
    (if (not binding)
        (error 'lookup "Variable ~a not declared" var)
        (let ([value (cdr binding)])
          (if (equal? value '*unassigned*)
              (error 'lookup "Variable ~a used before assignment" var)
              value))))))

  
(define (lookup state var)
  (let ([binding (assoc var state)])
    (if (not binding)
        (error 'lookup "Variable ~a not declared" var)
        (let ([value (cdr binding)])
          (if (equal? value '*unassigned*)
              (error 'lookup "Variable ~a used before assignment" var)
              value)))))

; Bind a new variable with an initial value (or mark as unassigned).
(define var_bind
  (lambda (state var val)
    (if (assoc var state)
      (error 'bind "Variable ~a already declared" var)
      (cons (cons var val) state))))
    
(define (bind state var val)
  (if (assoc var state)
      (error 'bind "Variable ~a already declared" var)
      (cons (cons var val) state)))

; Update an existing variable’s value in the state.
(define var_update
  (lambda (state var val)
    (if (not (assoc var state))
      (error 'update "Variable ~a not declared" var)
      (map (lambda (pair) (if (equal? (car pair) var) (cons var val) pair)) state))))
    
(define (update state var val)
  (if (not (assoc var state))
      (error 'update "Variable ~a not declared" var)
      (map (lambda (pair) (if (equal? (car pair) var) (cons var val) pair)) state)))


; PROCESSING STATEMENTS

; Evaluate arithmetic expressions
(define expr_eval
  (lambda (expr state)
    (cond
    ; Numbers evaluate to themselves
    ((number? expr) expr)

    ; Recognize true and false as constants
    ((equal? expr 'true) #t)
    ((equal? expr 'false) #f)

    ; Variables: Lookup their value
    ((symbol? expr) (state_lookup state expr))

    ; Unary negation (-expr)
    ((and (list? expr) (= (length expr) 2) (equal? (car expr) '-))
     (- (eval-expr (cadr expr) state)))

    ; Logical NOT (!expr)
    ((and (list? expr) (= (length expr) 2) (equal? (car expr) '!))
     (not (eval-expr (cadr expr) state)))

    ; Binary arithmetic, comparison, and boolean operations
    ((and (list? expr) (= (length expr) 3))
     (let* ((op (car expr))
            (left (eval-expr (cadr expr) state))
            (right (eval-expr (caddr expr) state)))
       (case op
         ; Arithmetic operators
         ((+) (+ left right))
         ((-) (- left right))
         ((*) (* left right))
         ((/) (if (zero? right) (error 'eval-expr "Division by zero") (quotient left right)))
         ((%) (modulo left right))

        ; Comparison operators
         ((<) (< left right))
         ((>) (> left right))
         ((<=) (<= left right))
         ((>=) (>= left right))
         ((==) (= left right))
         ((!=) (not (= left right)))

         ; Boolean logic
         ((&&) (and left right))
         ((||) (or left right))

         (else (error 'eval-expr "Unknown operator: ~a" op)))))

    ; Invalid expressions
    (else (error 'eval-expr "Unknown expression: ~a" expr)))))

    
 (define (eval-expr expr state)
  (cond
    ; Numbers evaluate to themselves
    ((number? expr) expr)

    ; Recognize true and false as constants
    ((equal? expr 'true) #t)
    ((equal? expr 'false) #f)

    ; Variables: Lookup their value
    ((symbol? expr) (lookup state expr))

    ; Unary negation (-expr)
    ((and (list? expr) (= (length expr) 2) (equal? (car expr) '-))
     (- (eval-expr (cadr expr) state)))

    ; Logical NOT (!expr)
    ((and (list? expr) (= (length expr) 2) (equal? (car expr) '!))
     (not (eval-expr (cadr expr) state)))

    ; Binary arithmetic, comparison, and boolean operations
    ((and (list? expr) (= (length expr) 3))
     (let* ((op (car expr))
            (left (eval-expr (cadr expr) state))
            (right (eval-expr (caddr expr) state)))
       (case op
         ; Arithmetic operators
         ((+) (+ left right))
         ((-) (- left right))
         ((*) (* left right))
         ((/) (if (zero? right) (error 'eval-expr "Division by zero") (quotient left right)))
         ((%) (modulo left right))

         ; Comparison operators
         ((<) (< left right))
         ((>) (> left right))
         ((<=) (<= left right))
         ((>=) (>= left right))
         ((==) (= left right))
         ((!=) (not (= left right)))

         ; Boolean logic
         ((&&) (and left right))
         ((||) (or left right))

         (else (error 'eval-expr "Unknown operator: ~a" op)))))

    ; Invalid expressions
    (else (error 'eval-expr "Unknown expression: ~a" expr))))

; Evaluate a the value of an arithmetic expression
(define int_eval
  (lambda (expression)
    (cond
      ((number? expression) expression)
      ((eq? '+ (operator expression)) (+ (firstoperand expression) (secondoperand expression)))
      ((eq? '- (operator expression)) (- (firstoperand expression) (secondoperand expression)))
      ((eq? '* (operator expression)) (* (firstoperand expression) (secondoperand expression)))
      ((eq? '/ (operator expression)) (quotient (firstoperand expression) (secondoperand expression)))
      ((eq? '% (operator expression)) (remainder (firstoperand expression) (secondoperand expression)))
      (else (error 'bad-op "Invalid operator")))))

; Abstractions
 
(define operator car)
(define firstoperand cadr)
(define secondoperand caddr)


; Evaluate a boolean expression
(define boolean_eval
  (lambda (expression)
    (cond
      ((number? expression) expression)
      ((eq? '< (operator expression)) (< (firstoperand expression) (secondoperand expression)))
      ((eq? '> (operator expression)) (> (firstoperand expression) (secondoperand expression)))
      ((eq? '<= (operator expression)) (<= (firstoperand expression) (secondoperand expression)))
      ((eq? '>= (operator expression)) (>= (firstoperand expression) (secondoperand expression)))
      ((eq? '== (operator expression)) (eq? (firstoperand expression) (secondoperand expression)))
      ((eq? '!= (operator expression)) (eq? (eq? (firstoperand expression) (secondoperand expression)) #f))
      ((eq? '&& (operator expression) (and firstoperand secondoperand)))
      ((eq? '|| (operator expression) (or (car expression) (car (cdr expression)))))
      (else (error 'bad-op "Invalid operator")))))
 


; Evaluate a single statement in the current state.
(define stmt-eval
  (lambda (stmt state)
    (cond
    ; Handle return statement: (return <expr>)
    ((and (list? stmt) (equal? (car stmt) 'return))
     (let ([val (eval-expr (cadr stmt) state)])
       (bind state 'return val)))

    ; Handle variable declaration: (var <variable>) or (var <variable> <value>)
    ((and (list? stmt) (equal? (car stmt) 'var))
     (if (= (length stmt) 2)
         (bind state (cadr stmt) '*unassigned*)  ;; Mark as unassigned
         (bind state (cadr stmt) (eval-expr (caddr stmt) state))))

    ; Handle assignment: (= <variable> <value>)
    ((and (list? stmt) (equal? (car stmt) '=))
     (update state (cadr stmt) (eval-expr (caddr stmt) state)))

    ; Handle if statements: (if <condition> <then-stmt> <optional-else-stmt>)
    ((and (list? stmt) (equal? (car stmt) 'if))
     (let* ((condition (eval-expr (cadr stmt) state)))
       (if condition
           (eval-stmt (caddr stmt) state) ; Execute `then` branch
           (if (= (length stmt) 4)
               (eval-stmt (cadddr stmt) state) ; Execute `else` branch if present
               state)))) ; No `else`, return unchanged state

    ; Handle while statements: (while <condition> <body>)
    ((and (list? stmt) (equal? (car stmt) 'while))
     (let loop ([current-state state])
       (if (eval-expr (cadr stmt) current-state)  ; Check condition
           (loop (eval-stmt (caddr stmt) current-state))  ; Execute body, repeat
           current-state)))  ; Exit loop when condition is false

    ; Unknown statement
    (else (error 'eval-stmt "Unknown statement: ~a" stmt)))))

  
(define (eval-stmt stmt state)
  (cond
    ; Handle return statement: (return <expr>)
    ((and (list? stmt) (equal? (car stmt) 'return))
     (let ([val (eval-expr (cadr stmt) state)])
       (bind state 'return val)))

    ; Handle variable declaration: (var <variable>) or (var <variable> <value>)
    ((and (list? stmt) (equal? (car stmt) 'var))
     (if (= (length stmt) 2)
         (bind state (cadr stmt) '*unassigned*)  ; Mark as unassigned
         (bind state (cadr stmt) (eval-expr (caddr stmt) state))))

    ; Handle assignment: (= <variable> <value>)
    ((and (list? stmt) (equal? (car stmt) '=))
     (update state (cadr stmt) (eval-expr (caddr stmt) state)))

    ; Handle if statements: (if <condition> <then-stmt> <optional-else-stmt>)
    ((and (list? stmt) (equal? (car stmt) 'if))
     (let* ((condition (eval-expr (cadr stmt) state)))
       (if condition
           (eval-stmt (caddr stmt) state) ;; Execute `then` branch
           (if (= (length stmt) 4)
               (eval-stmt (cadddr stmt) state) ;; Execute `else` branch if present
               state)))) ;; No `else`, return unchanged state

    ; Handle while statements: (while <condition> <body>)
    ((and (list? stmt) (equal? (car stmt) 'while))
     (let loop ([current-state state])
       (if (eval-expr (cadr stmt) current-state)  ;; Check condition
           (loop (eval-stmt (caddr stmt) current-state))  ;; Execute body, repeat
           current-state)))  ;; Exit loop when condition is false

    ; Unknown statement
    (else (error 'eval-stmt "Unknown statement: ~a" stmt))))





; Evaluate a list of statements sequentially.
(define stmts_eval
  (lambda (stmts state)
    (cond
    ((null? stmts) state)
    (else
     (let ([new-state (eval-stmt (car stmts) state)])
       (if (assoc 'return new-state)   ; Stop if return encountered
           new-state
           (eval-statements (cdr stmts) new-state)))))))
  
  
(define (eval-statements stmts state)
  (cond
    ((null? stmts) state)
    (else
     (let ([new-state (eval-stmt (car stmts) state)])
       (if (assoc 'return new-state)   ; Stop if return encountered
           new-state
           (eval-statements (cdr stmts) new-state))))))


; MAIN FUNCTIONS

(define return
  (lambda (val)
    (cond
    ((equal? val #t) 'true) 
    ((equal? val #f) 'false) 
    (else val))))             

    
(define (normalize-return val)
  (cond
    ((equal? val #t) 'true)  
    ((equal? val #f) 'false) 
    (else val)))             

(define interpret
  (lambda (filename)
    (let* ([parse-tree (parser filename)]
         [initial-state (make-empty-state)]
         [final-state (eval-statements parse-tree initial-state)])
    (normalize-return (lookup final-state 'return)))))

(define (interprett filename)
  (let* ([parse-tree (parser filename)]
         [initial-state (make-empty-state)]
         [final-state (eval-statements parse-tree initial-state)])
    (normalize-return (lookup final-state 'return))))

#lang racket

#lang racket

; Myah Potter and Ryan Lin

(require "simpleParser.rkt")
(require "lex.rkt")


;---------------------------------------------------------------
; State Management
;---------------------------------------------------------------
(define make-empty-state
  (lambda () '()))

(define lookup
  (lambda (state var)
    ((lambda (binding)
       (if (not binding)
           (error 'lookup "Variable ~a not declared" var)
           ((lambda (value)
              (if (equal? value '*unassigned*)
                  (error 'lookup "Variable ~a used before assignment" var)
                  value))
            (cdr binding))))
     (my-assoc var state))))

(define bind
  (lambda (state var val)
    (if (my-assoc var state)
        (error 'bind "Variable ~a already declared" var)
        (cons (cons var val) state))))

(define my-assoc
  (lambda (key alist)
    (cond
      ((null? alist) #f)
      ((equal? (car (car alist)) key) (car alist))
      (else (my-assoc key (cdr alist))))))

(define update
  (lambda (state var val)
    (if (not (my-assoc var state))
        (error 'update "Variable ~a not declared" var)
        (map (lambda (pair)
               (if (equal? (car pair) var)
                   (cons var val)
                   pair))
             state))))

;---------------------------------------------------------------
; Processing: Expression Evaluation
;---------------------------------------------------------------
(define eval-expr
  (lambda (expr state)
    (cond
      ((number? expr) expr)
      ((equal? expr 'true) #t)
      ((equal? expr 'false) #f)
      ((symbol? expr) (lookup state expr))
      ((and (list? expr) (= (length expr) 2) (equal? (car expr) '-))
       (- (eval-expr (cadr expr) state)))
      ((and (list? expr) (= (length expr) 2) (equal? (car expr) '!))
       (not (eval-expr (cadr expr) state)))
      ((and (list? expr) (= (length expr) 3))
       ((lambda (op left right)
          (case op
            ((+) (+ left right))
            ((-) (- left right))
            ((*) (* left right))
            ((/) (if (zero? right)
                     (error 'eval-expr "Division by zero")
                     (quotient left right)))
            ((%) (modulo left right))
            ((<) (< left right))
            ((>) (> left right))
            ((<=) (<= left right))
            ((>=) (>= left right))
            ((==) (= left right))
            ((!=) (not (= left right)))
            ((&&) (and left right))
            ((||) (or left right))
            (else (error 'eval-expr "Unknown operator: ~a" op))))
        (car expr)
        (eval-expr (cadr expr) state)
        (eval-expr (caddr expr) state)))
      (else (error 'eval-expr "Unknown expression: ~a" expr)))))

;---------------------------------------------------------------
; Processing: Statement Evaluation
;---------------------------------------------------------------
(define eval-stmt
  (lambda (stmt state)
    (cond
      ((and (list? stmt) (equal? (car stmt) 'return))
       ((lambda (val)
          (bind state 'return val))
        (eval-expr (cadr stmt) state)))
      ((and (list? stmt) (equal? (car stmt) 'var))
       (if (= (length stmt) 2)
           (bind state (cadr stmt) '*unassigned*)
           (bind state (cadr stmt)
                 (eval-expr (caddr stmt) state))))
      ((and (list? stmt) (equal? (car stmt) '=))
       (update state (cadr stmt)
               (eval-expr (caddr stmt) state)))
      ((and (list? stmt) (equal? (car stmt) 'if))
       ((lambda (condition)
          (if condition
              (eval-stmt (caddr stmt) state)
              (if (= (length stmt) 4)
                  (eval-stmt (cadddr stmt) state)
                  state)))
        (eval-expr (cadr stmt) state)))
      ((and (list? stmt) (equal? (car stmt) 'while))
       (while-loop state stmt))
      (else (error 'eval-stmt "Unknown statement: ~a" stmt)))))

; Define a helper function for while loops.
(define while-loop
  (lambda (state stmt)
    (if (eval-expr (cadr stmt) state)
        (while-loop (eval-stmt (caddr stmt) state) stmt)
        state)))

(define eval-statements
  (lambda (stmts state)
    (if (null? stmts)
        state
        ((lambda (new-state)
           (if (my-assoc 'return new-state)
               new-state
               (eval-statements (cdr stmts) new-state)))
         (eval-stmt (car stmts) state)))))

;---------------------------------------------------------------
; Main Functionality
;---------------------------------------------------------------
(define normalize-return
  (lambda (val)
    (cond
      ((equal? val #t) 'true)
      ((equal? val #f) 'false)
      (else val))))

(define interpret
  (lambda (filename)
    ((lambda (parse-tree)
       ((lambda (initial-state)
          ((lambda (final-state)
             (normalize-return (lookup final-state 'return)))
           (eval-statements parse-tree initial-state)))
        (make-empty-state)))
     (parser filename))))
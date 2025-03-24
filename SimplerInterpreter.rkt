#lang racket

; Myah Potter & Ryan Lin

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


; implement layers
; implement throw
; implement break
; implement continue
; implement return
; make sure tail recursion is used for m state functions (only m state)
; implement boxes



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
    (cond
      ((eq? (my-assoc var state) #t) (error 'bind "Variable ~a already declared" var))
      ((eq? '() state) (cons (cons (cons var val) '()) state))
      ;((list? (car state)) (append (cons var val) (car state))))))
      (else (cons (cons (cons var val) (car state)) (cdr state))))))

(define my-assoc
  (lambda (key alist)
    (cond
      ((null? alist) #f)
      ((assoc key (car alist)) (assoc key (car alist))) ;((list? alist) (my-assoc key (car alist)) (my-assoc key (cdr alist)))
      (else (my-assoc key (cdr alist))))))

(define update
  (lambda (state var val)
    (if (not (my-assoc var state))
        (error 'update "Variable ~a not declared" var)
        (map (lambda (layer)
          (map (lambda (pair)
               (if (equal? (car pair) var)
                   (cons var val)
                   pair))
               layer))
             state))))

(define add-layer
  (lambda (state)
    (cons '() state)))
      
(define remove-layer
  (lambda (state)
    (cond
      ((null? state) state)
      ((list? (car state)) (cdr state)))))


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
  (lambda (stmt state return)
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
              (eval-stmt (caddr stmt) state return)
              (if (= (length stmt) 4)
                  (eval-stmt (cadddr stmt) state return)
                  state)))
        (eval-expr (cadr stmt) state)))
      ((and (list? stmt) (equal? (car stmt) 'while))
       (while-loop state stmt))
      ((equal? (car stmt) 'break) (eval-stmt (cdr stmt) (remove-layer state) return))
      ((equal? (car stmt) 'begin) (eval-stmt (cdr stmt) (add-layer state) return))
      (else (error 'eval-stmt "Unknown statement: ~a" stmt)))))

(define eval-special-stmt
  (lambda (stmt state return)
    (cond
      ((equal? (car stmt) 'begin) (eval-special-stmt (cdr stmt) (add-layer stmt) return))
      ((equal? (car stmt) 'break) (eval-special-stmt (cdr stmt) (remove-layer stmt) return))
      ((equal? (car stmt) 'throw) (return))
      ((equal? (car stmt) 'try) (return))
      ((equal? (car stmt) 'catch) (return))
      ((equal? (car stmt) 'continue) (return))
      ((equal? (car stmt) 'return) (return)))))

; Updated eval-stmt function to change states based using statement(s)
(define eval-stmtx
  (lambda (stmt state return)
    (cond
      ((list? (car stmt)) (eval-stmt (car stmt) state (lambda (v1) (eval-stmt (cdr stmt) (lambda (v2) (return (cons v1 v2)))))))
      ((equal? (car stmt) 'begin) (eval-stmt (cdr stmt) (add-layer state) return))
      ;((equal? (car stmt) #\}) (eval-stmt (cdr stmt) (remove-layer state) return))
      ((and (list? stmt) (equal? (car stmt) 'var))
      (if (= (length stmt) 2)
           (bind state (cadr stmt) '*unassigned*)
           (bind state (cadr stmt)
                 (eval-expr (caddr stmt) state))))
      ((equal? (car stmt) '=) (eval-stmt (cdr stmt) (bind state (cadr stmt) (caadr stmt)) return))
      ((equal? (car stmt) 'return) (bind 'return (cadr stmt)) (eval-expr (cadr stmt) state))
      ((equal? (car stmt) 'break) (eval-stmt (cdr stmt) (remove-layer state) return))
      ((equal? (car stmt) 'throw) (return))
      ((equal? (car stmt) 'continue) (return))
      ((equal? (car stmt) 'if) (if-stmt state stmt) (eval-expr (cadr stmt) state))
      ((equal? (car stmt) 'while) (while-loop state stmt))
      (else `(error 'eval-stmt "Unknown statement: ~a" stmt)))))


; Define a helper function for while loops.
(define while-loop
  (lambda (state stmt)
    (if (eval-expr (cadr stmt) state)
        (while-loop (eval-stmt (caddr stmt) state) stmt)
        state)))

; Define a helper for if statements.
(define if-stmt
 (lambda (state stmt)
    (if (equal? (cadr stmt) #t)
      (eval-stmt (caadr stmt) state)
       (if (= (length stmt) 4)
         (eval-stmt (cadddr stmt) state) state))))

(define eval-statements
  (lambda (stmts state)
    (if (null? stmts)
        state
        ((lambda (new-state)
           (if (my-assoc 'return new-state)
               new-state
               (eval-statements (cdr stmts) new-state)))
         (eval-stmt (car stmts) state (lambda (v) v))))))

;---------------------------------------------------------------
; Abstractions
;---------------------------------------------------------------



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

#lang racket

; Myah Potter and Ryan Lin

(require "simpleParser.rkt")
(require "lex.rkt")


;---------------------------------------------------------------
; State Management (Layered)
;---------------------------------------------------------------
(define make-empty-state (lambda () '(())))

(define my-assoc
  (lambda (key alist)
    (cond
      ((null? alist) #f)
      ((equal? (car (car alist)) key) (car alist))
      (else (my-assoc key (cdr alist))))))

(define lookup
  (lambda (state var)
    (let loop ((layers state))
      (cond
        ((null? layers)
         (error 'lookup "Variable ~a not declared" var))
        ((my-assoc var (car layers)) =>
         (lambda (binding)
           (let ((val (cdr binding)))
             (if (equal? val '*unassigned*)
                 (error 'lookup "Variable ~a used before assignment" var)
                 val))))
        (else (loop (cdr layers)))))))

(define bind
  (lambda (state var val)
    (if (my-assoc var (car state))
        (error 'bind "Variable ~a already declared in this scope" var)
        (cons (cons var val) (car state)))))

(define push-binding
  (lambda (state var val)
    (cons (cons var val) (car state))))

(define update
  (lambda (state var val)
    (let loop ((layers state) (acc '()))
      (cond
        ((null? layers)
         (error 'update "Variable ~a not declared" var))
        ((my-assoc var (car layers))
         (let* ((updated-layer
                 (map (lambda (pair)
                        (if (equal? (car pair) var)
                            (cons var val)
                            pair))
                      (car layers))))
           (append (reverse acc) (cons updated-layer (cdr layers)))))
        (else (loop (cdr layers) (cons (car layers) acc)))))))


;---------------------------------------------------------------
; Expression Evaluation
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
; Statement Evaluation
;---------------------------------------------------------------
(define eval-stmt
  (lambda (stmt state return-cont continue-cont break-cont)
    (cond
      ((and (list? stmt) (equal? (car stmt) 'return))
       (return-cont (eval-expr (cadr stmt) state)))

      ((and (list? stmt) (equal? (car stmt) 'continue))
       (continue-cont state))

      ((and (list? stmt) (equal? (car stmt) 'break))
       (break-cont state))

      ((and (list? stmt) (equal? (car stmt) 'var))
       (let ((var (cadr stmt))
             (val (if (= (length stmt) 2) '*unassigned*
                      (eval-expr (caddr stmt) state))))
         (cons (cons (cons var val) (car state)) (cdr state))))

      ((and (list? stmt) (equal? (car stmt) '=))
       (update state (cadr stmt)
               (eval-expr (caddr stmt) state)))

      ((and (list? stmt) (equal? (car stmt) 'if))
       (if (eval-expr (cadr stmt) state)
           (eval-stmt (caddr stmt) state return-cont continue-cont break-cont)
           (if (= (length stmt) 4)
               (eval-stmt (cadddr stmt) state return-cont continue-cont break-cont)
               state)))

      ((and (list? stmt) (equal? (car stmt) 'while))
       (call/cc
        (lambda (break-cont-inner)
          (define (loop current-state)
            (if (eval-expr (cadr stmt) current-state)
                (call/cc
                 (lambda (continue-cont-inner)
                   (let ((new-state (eval-stmt (caddr stmt) current-state return-cont continue-cont-inner break-cont-inner)))
                     (loop new-state))))
                current-state))
          (loop state))))

      ((and (list? stmt) (equal? (car stmt) 'begin))
       (let* ((new-layer '())
              (new-state (cons new-layer state))
              (final-state (eval-statements (cdr stmt) new-state return-cont continue-cont break-cont)))
         (cdr final-state)))

      (else (error 'eval-stmt "Unknown statement: ~a" stmt)))))


(define eval-statements
  (lambda (stmts state return-cont continue-cont break-cont)
    (if (null? stmts)
        state
        (let ((new-state (eval-stmt (car stmts) state return-cont continue-cont break-cont)))
          (eval-statements (cdr stmts) new-state return-cont continue-cont break-cont)))))


;---------------------------------------------------------------
; Main
;---------------------------------------------------------------
(define normalize-return
  (lambda (val)
    (cond
      ((equal? val #t) 'true)
      ((equal? val #f) 'false)
      (else val))))

(define interpret
  (lambda (filename)
    (let ((parse-tree (parser filename)))
      (call/cc
       (lambda (return-cont)
         (let ((initial-state (make-empty-state)))
           (eval-statements parse-tree initial-state return-cont (lambda (s) s) (lambda (s) s)))))))
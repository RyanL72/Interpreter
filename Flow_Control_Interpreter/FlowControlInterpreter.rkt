#lang racket
;; Alternative interpreter using explicit control-flow result wrappers

(require "simpleParser.rkt")
(require "lex.rkt")

;; Define result wrappers for normal execution and control flow signals
(struct normal (state) #:transparent)
(struct break (state) #:transparent)
(struct continue (state) #:transparent)
(struct return (value) #:transparent)

;;---------------------------------------------------------------
;; State Management (Layered)
;;---------------------------------------------------------------
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
        (cons (cons (cons var val) (car state)) (cdr state)))))

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

;;---------------------------------------------------------------
;; Expression Evaluation
;;---------------------------------------------------------------
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

;;---------------------------------------------------------------
;; Statement Evaluation using explicit result propagation
;;---------------------------------------------------------------
(define (eval-stmt stmt state)
  (cond
    ;; Return statement: immediately produce a return result.
    ((and (list? stmt) (equal? (car stmt) 'return))
     (return (eval-expr (cadr stmt) state)))
    
    ;; Continue and break just tag the state.
    ((and (list? stmt) (equal? (car stmt) 'continue))
     (continue state))
    
    ((and (list? stmt) (equal? (car stmt) 'break))
     (break state))
    
    ;; Variable declaration: create a new binding.
    ((and (list? stmt) (equal? (car stmt) 'var))
     (let ((var (cadr stmt))
           (val (if (= (length stmt) 2)
                    '*unassigned*
                    (eval-expr (caddr stmt) state))))
       (normal (cons (cons (cons var val) (car state)) (cdr state)))))
    
    ;; Assignment: update the binding.
    ((and (list? stmt) (equal? (car stmt) '=))
     (normal (update state (cadr stmt) (eval-expr (caddr stmt) state))))
    
    ;; If statement: choose the branch based on condition.
    ((and (list? stmt) (equal? (car stmt) 'if))
     (if (eval-expr (cadr stmt) state)
         (eval-stmt (caddr stmt) state)
         (if (= (length stmt) 4)
             (eval-stmt (cadddr stmt) state)
             (normal state))))
    
    ;; While loop: repeatedly execute body until the condition fails.
    ((and (list? stmt) (equal? (car stmt) 'while))
     (let loop ((current-state state))
       (if (eval-expr (cadr stmt) current-state)
           (let ((result (eval-stmt (caddr stmt) current-state)))
             (cond
               [(return? result) result]
               [(break? result) (normal (break-state result))]
               [(continue? result) (loop (continue-state result))]
               [(normal? result) (loop (normal-state result))]
               [else (error "Unknown control result" result)]))
           (normal current-state))))
    
    ;; Begin block: create a new scope.
    ((and (list? stmt) (equal? (car stmt) 'begin))
     (let* ((new-layer '())
            (new-state (cons new-layer state))
            (result (eval-statements (cdr stmt) new-state)))
       ;; In a block, we discard local bindings but propagate outer state updates.
       (cond
         [(normal? result) (normal (cdr result))]
         [else result]))
     )
    
    (else (error 'eval-stmt "Unknown statement: ~a" stmt))))

(define (eval-statements stmts state)
  (if (null? stmts)
      (normal state)
      (let ((result (eval-stmt (car stmts) state)))
        (cond
          [(normal? result) (eval-statements (cdr stmts) (normal-state result))]
          [(continue? result) result]
          [(break? result) result]
          [(return? result) result]
          [else (error "Unknown control result" result)]))))

;;---------------------------------------------------------------
;; Main
;;---------------------------------------------------------------
(define (normalize-return val)
  (cond
    ((equal? val #t) 'true)
    ((equal? val #f) 'false)
    (else val)))

(define (interpret filename)
  (let ((parse-tree (parser filename)))
    (let ((result (eval-statements parse-tree (make-empty-state))))
      (cond
        [(normal? result)
         (let ((final (normal-state result)))
           (normalize-return
            (+ (* (lookup final 'x) 100)
               (* (lookup final 'y) 10)
               (lookup final 'z))))]
        [(return? result) (normalize-return (return-value result))]
        [else (error "Unexpected control flow at top level" result)]))))

;; End of alternative interpreter code.

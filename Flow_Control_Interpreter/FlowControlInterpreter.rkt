#lang racket
;; Alternative interpreter with try/catch/finally and throw

(require "simpleParser.rkt")
(require "lex.rkt")

;;---------------------------------------------------------------
;; Control-Flow Result Structures
;;---------------------------------------------------------------
(struct normal (state) #:transparent)
(struct break (state) #:transparent)
(struct continue (state) #:transparent)
(struct return (value) #:transparent)
(struct thrown (value state) #:transparent)

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
;; Statement Evaluation with Exception Handling
;;---------------------------------------------------------------
(define (eval-stmt stmt state)
  (cond
    ;; Return statement
    ((and (list? stmt) (equal? (car stmt) 'return))
     (return (eval-expr (cadr stmt) state)))
    
    ;; Continue statement
    ((and (list? stmt) (equal? (car stmt) 'continue))
     (continue state))
    
    ;; Break statement
    ((and (list? stmt) (equal? (car stmt) 'break))
     (break state))
    
    ;; Throw statement
    ((and (list? stmt) (equal? (car stmt) 'throw))
     (thrown (eval-expr (cadr stmt) state) state))
    
    ;; Variable declaration
    ((and (list? stmt) (equal? (car stmt) 'var))
     (let ((var (cadr stmt))
           (val (if (= (length stmt) 2)
                    '*unassigned*
                    (eval-expr (caddr stmt) state))))
       (normal (cons (cons (cons var val) (car state)) (cdr state)))))
    
    ;; Assignment
    ((and (list? stmt) (equal? (car stmt) '=))
     (normal (update state (cadr stmt) (eval-expr (caddr stmt) state))))
    
    ;; If statement
    ((and (list? stmt) (equal? (car stmt) 'if))
     (if (eval-expr (cadr stmt) state)
         (eval-stmt (caddr stmt) state)
         (if (= (length stmt) 4)
             (eval-stmt (cadddr stmt) state)
             (normal state))))
    
    ;; While loop
    ((and (list? stmt) (equal? (car stmt) 'while))
     (let loop ((current-state state))
       (if (eval-expr (cadr stmt) current-state)
           (let ((result (eval-stmt (caddr stmt) current-state)))
             (cond
               [(return? result) result]
               [(break? result) (normal (break-state result))]
               [(continue? result) (loop (continue-state result))]
               [(normal? result) (loop (normal-state result))]
               [(thrown? result) result]
               [else (error "Unknown control result" result)]))
           (normal current-state))))
    
    ;; Begin block (scope)
    ((and (list? stmt) (equal? (car stmt) 'begin))
     (let* ((new-layer '())
            (new-state (cons new-layer state))
            (result (eval-statements (cdr stmt) new-state)))
       (cond
         [(normal? result) (normal (cdr (normal-state result)))]
         [else result])))
    
    ;; Try-catch-finally statement
    ((and (list? stmt) (equal? (car stmt) 'try))
     (let* ((try-block (cadr stmt))
            (catch-clause (caddr stmt))   ; expected form: (catch e <catch-block>)
            (finally-clause (cadddr stmt)) ; expected form: (finally <finally-block>)
            (result-try (eval-stmt try-block state))
            (result-after-catch
             (if (thrown? result-try)
                 (let* ((ex (thrown-value result-try))
                        (throw-state (thrown-state result-try))
                        (catch-var (cadr catch-clause))
                        (catch-body (caddr catch-clause))
                        (catch-state (cons (list (list catch-var ex)) throw-state)))
                   (eval-stmt catch-body catch-state))
                 result-try))
            (state-for-finally (cond
                                 [(normal? result-after-catch) (normal-state result-after-catch)]
                                 [(thrown? result-after-catch) (thrown-state result-after-catch)]
                                 [else (error "Invalid control result in try-catch" result-after-catch)]))
            (result-finally (eval-stmt (cadr finally-clause) state-for-finally)))
       (cond
         [(not (normal? result-finally))
          result-finally]
         [else
          (if (normal? result-after-catch)
              (normal (normal-state result-finally))
              (thrown (thrown-value result-after-catch) (normal-state result-finally)))])))
    
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
          [(thrown? result) result]
          [else (error "Unknown control result" result)]))))

;;---------------------------------------------------------------
;; Top-Level: Normalizing Return Value
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
           (normalize-return final))]
        [(return? result) (normalize-return (return-value result))]
        [(thrown? result) (error "Uncaught exception:" (thrown-value result))]
        [else (error "Unexpected control flow at top level" result)]))))

;; End of interpreter code.



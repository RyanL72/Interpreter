#lang racket
(require "functionParser.rkt")
; (load "functionParser.scm")

; An interpreter for the simple language using tail recursion for the M_state functions and does not handle side effects.

; The functions that start interpret-...  all return the current environment.  These are the M_state functions.
; The functions that start eval-...  all return a value.  These are the M_value and M_boolean functions.

; The main function. 
(define interpret
  (lambda (filename)
    (let* ((program (parser filename))
           (global-env (interpret-top-level program (newenvironment)))
           (main-func (lookup 'main global-env)))
      ; (printf "global environment : ~a" global-env) ; DEBUG
      (scheme->language (call-function main-func '() global-env)))))

; interprets a list of statements.  The state/environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw next)
    (if (null? statement-list)
        (next environment)
        (interpret-statement (car statement-list) environment return break continue throw
                             (lambda (env)
                               (interpret-statement-list (cdr statement-list) env return break continue throw next))))))

(define interpret-top-level
  (lambda (top-level-list environment)
    (if (null? top-level-list)
        environment
        (let* ((stmt (car top-level-list))
               (new-env
                (cond
                  ((eq? (car stmt) 'var)
                   (interpret-declare stmt environment (lambda (e) e)))
                  ((eq? (car stmt) 'function)
                   (let* ((name (cadr stmt))
                          (params (caddr stmt))
                          (body (cadddr stmt))
                          (closure (make-function-closure name params body environment)))
                     (insert name closure environment)))
                  (else (myerror "Invalid top-level statement:" stmt)))))
          (interpret-top-level (cdr top-level-list) new-env)))))

(define make-function-closure
  (lambda (name params body defining-env)
    (list 'closure name params body defining-env)))

(define call-function
  (lambda (closure arg-values call-env)
    ; (printf "calling ~a with ~a in ~a" closure arg-values call-env) ; DEBUG
    (let* ((closure-name (list-ref closure 1))
           (closure-params (list-ref closure 2))
           (closure-body (list-ref closure 3))
           (closure-env (list-ref closure 4)))
      ;; Check that the number of arguments matches the number of parameters.
      (if (not (= (length closure-params) (length arg-values)))
          (myerror "wrong number of parameters: expected" (length closure-params)
                   "but received" (length arg-values))
          (let* ((self-env (insert closure-name closure closure-env))
                 (func-env (extend-environment closure-params arg-values self-env)))
            (interpret-statement-list closure-body func-env
                                      (lambda (v) v)
                                      (lambda (env) (myerror "Break used outside of loop"))
                                      (lambda (env) (myerror "Continue used outside of loop"))
                                      (lambda (v env) (myerror "Uncaught exception"))
                                      (lambda (env) 'novalue)))))))

(define extend-environment
  (lambda (params args parent-env)
    (let ((new-env (push-frame parent-env)))
      (extend-env-helper params args new-env))))

(define extend-env-helper
  (lambda (params args env)
    (if (null? params)
        env
        (extend-env-helper (cdr params) (cdr args)
                           (insert (car params) (car args) env)))))

; interpret a statement in the environment with continuations for return, break, continue, throw, and "next statement"
(define interpret-statement
  (lambda (statement environment return break continue throw next)
    (cond
      ((eq? 'return (statement-type statement))
       (interpret-return statement environment return))
      ((eq? 'var (statement-type statement))
       (interpret-declare statement environment next))
      ((eq? 'function (statement-type statement))    ; <-- Handle inner function definitions
       (let* ((name (cadr statement))
              (params (caddr statement))
              (body (cadddr statement))
              (closure (make-function-closure name params body environment)))
         (next (insert name closure environment))))
      ((eq? '= (statement-type statement))
       (interpret-assign statement environment next))
      ((eq? 'if (statement-type statement))
       (interpret-if statement environment return break continue throw next))
      ((eq? 'while (statement-type statement))
       (interpret-while statement environment return throw next))
      ((eq? 'continue (statement-type statement))
       (continue environment))
      ((eq? 'break (statement-type statement))
       (break environment))
      ((eq? 'begin (statement-type statement))
       (interpret-block statement environment return break continue throw next))
      ((eq? 'throw (statement-type statement))
       (interpret-throw statement environment throw))
      ((eq? 'try (statement-type statement))
       (interpret-try statement environment return break continue throw next))
      ((eq? 'funcall (statement-type statement))
       (eval-expression statement environment)
       (next environment))
      (else (myerror "Unknown statement:" (statement-type statement))))))

; Calls the return continuation with the given expression value.
(define interpret-return
  (lambda (statement environment return)
    (return (eval-expression (get-expr statement) environment))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable.
(define interpret-declare
  (lambda (statement environment next)
    (if (exists-declare-value? statement)
        (next (insert (get-declare-var statement)
                      (eval-expression (get-declare-value statement) environment)
                      environment))
        (next (insert (get-declare-var statement) 'novalue environment)))))

; Updates the environment to add a new binding for a variable.
(define interpret-assign
  (lambda (statement environment next)
    (next (update (get-assign-lhs statement)
                  (eval-expression (get-assign-rhs statement) environment)
                  environment))))

; Check if there is an else condition; if not, evaluate the condition and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw next)
    (cond
      ((eval-expression (get-condition statement) environment)
       (interpret-statement (get-then statement) environment return break continue throw next))
      ((exists-else? statement)
       (interpret-statement (get-else statement) environment return break continue throw next))
      (else (next environment)))))

; Interprets a while loop.  We must create break and continue continuations for this loop.
(define interpret-while
  (lambda (statement environment return throw next)
    (letrec ((loop (lambda (condition body environment)
                     (if (eval-expression condition environment)
                         (interpret-statement body environment
                                              return
                                              (lambda (env) (next env))
                                              (lambda (env) (loop condition body env))
                                              throw
                                              (lambda (env) (loop condition body env)))
                         (next environment)))))
      (loop (get-condition statement) (get-body statement) environment))))

; Interprets a block.  The break, continue, throw and "next statement" continuations must be adjusted to pop the environment.
(define interpret-block
  (lambda (statement environment return break continue throw next)
    (interpret-statement-list (cdr statement)
                              (push-frame environment)
                              return
                              (lambda (env) (break (pop-frame env)))
                              (lambda (env) (continue (pop-frame env)))
                              (lambda (v env) (throw v (pop-frame env)))
                              (lambda (env) (next (pop-frame env))))))

; Uses a continuation to throw the proper value.  Because we are not using boxes, the environment/state must be thrown as well so any changes are preserved.
(define interpret-throw
  (lambda (statement environment throw)
    (throw (eval-expression (get-expr statement) environment) environment)))

; ;;---------------------------------------------------------------
; Try-Catch-Finally support
; 
; In a try statement our language expects the structure:
;   (try <try-block> (<catch> (<var>) <catch-body>) (<finally> <finally-body>))
;
; interpret-try sets up new continuations for return, break, continue, and throw.
; The new throw continuation is produced by create-throw-catch-continuation.
; 
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw next finally-block)
    (cond
      ((null? catch-statement)
       (lambda (ex env)
         (interpret-block finally-block env return break continue throw
                          (lambda (env2) (throw ex env2)))))
      ((not (eq? 'catch (statement-type catch-statement)))
       (myerror "Incorrect catch statement"))
      (else
       (lambda (ex env)
         (let ((catch-result
                (interpret-statement-list
                 (get-body catch-statement)
                 (insert (catch-var catch-statement) ex (push-frame env))
                 return
                 (lambda (env2) (break (pop-frame env2)))
                 (lambda (env2) (continue (pop-frame env2)))
                 (lambda (v env2) (throw v (pop-frame env2)))
                 (lambda (env2) 
                   ; After the catch body finishes, execute the finally block and then return catch-result.
                   (interpret-block finally-block (pop-frame env2) return break continue throw next)))))
           catch-result))))))
           
(define interpret-try
  (lambda (statement environment return break continue throw next)
    (let* ((finally-block (make-finally-block (get-finally statement)))
           (try-block (make-try-block (get-try statement)))
           (new-return (lambda (v)
                         (interpret-block finally-block environment return break continue throw
                                          (lambda (env2) (return v)))))
           (new-break (lambda (env)
                        (interpret-block finally-block env return break continue throw
                                         (lambda (env2) (break env2)))))
           (new-continue (lambda (env)
                           (interpret-block finally-block env return break continue throw
                                            (lambda (env2) (continue env2)))))
           (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw next finally-block)))
      (interpret-block try-block environment new-return new-break new-continue new-throw
                       (lambda (env) (interpret-block finally-block env return break continue throw next))))))

; helper methods to reuse the interpret-block method on try and finally blocks.
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally))
       (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; ;;---------------------------------------------------------------
; Expression Evaluation
; 
; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr environment))
      (else (eval-operator expr environment)))))

; Evaluate a binary (or unary) operator. Evaluates the left operand first then delegates to eval-binary-op2 for the right operand,
; ensuring the proper order in case side effects are added.
(define eval-operator
  (lambda (expr environment)
    (cond
      ;; Handle a function call.
      ((eq? (operator expr) 'funcall)
       (let* ((fun (lookup (cadr expr) environment))
              (args (map (lambda (arg)
                           (eval-expression arg environment))
                         (cddr expr))))
         (call-function fun args environment)))
      ;; Handle logical not.
      ((eq? (operator expr) '!)
       (not (eval-expression (operand1 expr) environment)))
      ;; Handle unary minus (e.g., -x).
      ((and (eq? (operator expr) '-) (= 2 (length expr)))
       (- (eval-expression (operand1 expr) environment)))
      ;; Otherwise delegate to binary operator evaluation.
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment) environment)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal. Special test for numbers versus booleans.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))

; ;;---------------------------------------------------------------
; Helper Functions for Expression Parsing
; 
; These helper functions define the operator and operands of a value expression.
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; Helper functions defining the parts of various statement types.
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))

; ;;---------------------------------------------------------------
; Environment/State Functions
; 
; With the mutable approach, a frame is implemented as a hash table mapping variable names to values.
; Create an empty frame.
(define newframe
  (lambda ()
    (make-hash)))

; Create a new empty environment.
(define newenvironment
  (lambda ()
    (list (newframe))))

; Add a frame onto the top of the environment.
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; Remove a frame from the environment.
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; Test if a variable exists in the environment.
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((hash-has-key? (car environment) var) #t)
      (else (exists? var (cdr environment))))))

; Look up a value in the environment. Converts booleans to the language format.
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))

; Helper function for lookup. Errors if variable is unassigned.
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? value 'novalue)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment.
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((hash-has-key? (car environment) var) (hash-ref (car environment) var))
      (else (lookup-in-env var (cdr environment))))))

; Alternative: Look up a value in a given frame.
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (hash-has-key? frame var))
       (myerror "error: undefined variable" var))
      (else (hash-ref frame var)))))

; Adds a new variable/value binding pair into the environment.
; If the variable is already defined in the current frame, it is updated.
(define insert
  (lambda (var val environment)
    (hash-set! (car environment) var val)
    environment))

; Changes the binding of a variable to a new value in the environment.
; Errors if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (begin (update-existing var val environment)
               environment)
        (myerror "error: variable used but not defined:" var))))

; Update an existing variable in the environment (searching frames in order) by modifying in place.
(define update-existing
  (lambda (var val environment)
    (if (hash-has-key? (car environment) var)
        (hash-set! (car environment) var val)
        (update-existing var val (cdr environment)))))

; Because the error function is not defined in R5RS Scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

; Updated myerror function: converts every argument to a string regardless of its type.
(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (acc vals)
                        (if (null? vals)
                            acc
                            (makestr (string-append acc " " (format "~a" (car vals))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

; Functions to convert the Scheme #t and #f to our language's true and false, and back.
(define language->scheme
  (lambda (v)
    (cond
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))



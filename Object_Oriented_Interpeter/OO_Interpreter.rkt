;; Object Oriented Interpreter
;; Myah Potter Ryan Lin

#lang racket

(require "classParser.rkt")

;; An interpreter for the simple language using tail recursion for the M_state functions and does not handle side effects.
;; The functions starting with interpret- return environments (M_state); functions starting with eval- return values (M_value and M_boolean).

;; Interpret a full program, locate the class's main method, and call it.
(define interpret
  (lambda (filename classname)
    (let* ((program (parser filename))
           (global-env (interpret-top-level program (newenvironment)))
           (class-closure (lookup classname global-env))
           (methods (list-ref class-closure 4))
           (main-func (hash-ref methods 'main)))
      (scheme->language
       (call-function main-func '() global-env)))))

;; Interpret a full program but print debugging information.
(define interpret-debug
  (lambda (filename classname)
    (let* ((program (parser filename))
           (global-env (interpret-top-level program (newenvironment))))
      (printf "Parsed Program: ~a\n" program)
      (printf "Class Name: ~a\n" classname)
      (printf "Global Environment: ~a\n" global-env)
      (printf "Class Call: ~a /n" (lookup classname global-env)))))

;; Interpret a list of statements sequentially with continuation passing.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw next)
    (if (null? statement-list)
        (next environment)
        (interpret-statement (car statement-list) environment return break continue throw
                             (lambda (env)
                               (interpret-statement-list (cdr statement-list) env return break continue throw next))))))

;; Interpret top-level program elements (e.g., classes).
(define interpret-top-level
  (lambda (top-level-list environment)
    (if (null? top-level-list)
        environment
        (let* ((stmt (car top-level-list))
               (new-env
                (cond
                  ((eq? (car stmt) 'class)
                   (let* ((class-name (cadr stmt))
                          (parent (caddr stmt))
                          (members (cadddr stmt))
                          (fields-methods (process-members members environment))
                          (fields (car fields-methods))
                          (methods (cadr fields-methods))
                          (class-closure
                           (list 'class-closure class-name parent fields methods)))
                     (insert class-name class-closure environment)))
                  (else (myerror "Invalid top-level statement:" stmt)))))
          (interpret-top-level (cdr top-level-list) new-env)))))

;; Process fields and methods inside a class definition.
(define process-members
  (lambda (members defining-env)
    (let ((fields (make-hash))
          (methods (make-hash)))
      (for-each
       (lambda (m)
         (cond
           ((eq? (car m) 'var)
            (hash-set! fields (cadr m) (caddr m)))
           ((or (eq? (car m) 'static-function) (eq? (car m) 'function))
            (hash-set! methods (cadr m)
                       (make-function-closure (cadr m) (caddr m) (cadddr m) defining-env)))))
       members)
      (list fields methods))))

;; Create a new instance closure from a class closure.
(define make-instance-closure
  (lambda (class-closure)
    (let* ((fields-template (list-ref class-closure 3))
           (fields (make-hash)))
      (for-each (lambda (k)
                  (hash-set! fields k (hash-ref fields-template k)))
                (hash-keys fields-template))
      (list 'instance-closure class-closure fields))))

;; Create a function closure with its environment.
(define make-function-closure
  (lambda (name params body defining-env)
    (list 'closure name params body defining-env)))

;; Call a method with a bound instance and arguments.
(define call-method
  (lambda (method-closure this-instance args call-env)
    (let* ((method-name (list-ref method-closure 1))
           (method-params (list-ref method-closure 2))
           (method-body (list-ref method-closure 3))
           (base-env (push-frame (newenvironment)))
           (self-env (insert 'this this-instance base-env))
           (func-env (extend-environment method-params args self-env)))
      (interpret-statement-list method-body func-env
                                (lambda (v) v)
                                (lambda (env) (myerror "Break outside loop"))
                                (lambda (env) (myerror "Continue outside loop"))
                                (lambda (v env) v)
                                (lambda (env) 'novalue)))))

;; Call a normal function closure with arguments.
(define call-function
  (lambda (closure arg-values call-env)
    (let* ((closure-name (list-ref closure 1))
           (closure-params (list-ref closure 2))
           (closure-body (list-ref closure 3))
           (closure-env (list-ref closure 4)))
      (if (not (= (length closure-params) (length arg-values)))
          (myerror "wrong number of parameters: expected" (length closure-params) "but received" (length arg-values))
          (let* ((self-env (insert closure-name closure closure-env))
                 (func-env (extend-environment closure-params arg-values self-env)))
            (interpret-statement-list closure-body func-env
                                      (lambda (v) v)
                                      (lambda (env) (myerror "Break used outside of loop"))
                                      (lambda (env) (myerror "Continue used outside of loop"))
                                      (lambda (v env) v)
                                      (lambda (env) 'novalue)))))))

;; Extend an environment by adding parameters bound to argument values.
(define extend-environment
  (lambda (params args parent-env)
    (let ((new-env (push-frame parent-env)))
      (extend-env-helper params args new-env))))

;; Helper for recursively binding parameters to arguments.
(define extend-env-helper
  (lambda (params args env)
    (if (null? params)
        env
        (extend-env-helper (cdr params) (cdr args)
                           (insert (car params) (car args) env)))))

;; Interpret a single statement based on its type.
(define interpret-statement
  (lambda (statement environment return break continue throw next)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment next))
      ((eq? 'function (statement-type statement))
       (let* ((name (cadr statement))
              (params (caddr statement))
              (body (cadddr statement))
              (closure (make-function-closure name params body environment)))
         (next (insert name closure environment))))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment next))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw next))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw next))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw next))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw next))
      ((eq? 'funcall (statement-type statement))
       (eval-expression statement environment)
       (next environment))
      (else (myerror "Unknown statement:" (statement-type statement))))))

;; Interpret a return statement by calling the return continuation with the evaluated expression.
(define interpret-return
  (lambda (statement environment return)
    (return (eval-expression (get-expr statement) environment))))

;; Interpret a variable declaration, with optional initialization.
(define interpret-declare
  (lambda (statement environment next)
    (if (exists-declare-value? statement)
        (next (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment) environment))
        (next (insert (get-declare-var statement) 'novalue environment)))))

;; Interpret an assignment to a variable or field.
(define interpret-assign
  (lambda (statement environment next)
    (let* ((lhs (get-assign-lhs statement))
           (rhs (eval-expression (get-assign-rhs statement) environment)))
      (cond
        ((symbol? lhs) (next (update lhs rhs environment)))
        ((and (list? lhs) (eq? (car lhs) 'dot) (eq? (operand1 lhs) 'this))
         (let* ((field-name (operand2 lhs))
                (this-instance (lookup 'this environment)))
           (unless (and (list? this-instance) (eq? (car this-instance) 'instance-closure))
             (myerror "this is not an instance:" this-instance))
           (let ((fields (list-ref this-instance 2)))
             (hash-set! fields field-name rhs)
             (next environment))))
        (else (myerror "invalid assignment lhs:" lhs))))))

;; Interpret an if-then-else statement.
(define interpret-if
  (lambda (statement environment return break continue throw next)
    (cond
      ((eval-expression (get-condition statement) environment)
       (interpret-statement (get-then statement) environment return break continue throw next))
      ((exists-else? statement)
       (interpret-statement (get-else statement) environment return break continue throw next))
      (else (next environment)))))

;; Interpret a while loop by setting up loop continuations.
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

;; Interpret a block by pushing a new frame and handling scoping.
(define interpret-block
  (lambda (statement environment return break continue throw next)
    (interpret-statement-list (cdr statement)
                              (push-frame environment)
                              return
                              (lambda (env) (break (pop-frame env)))
                              (lambda (env) (continue (pop-frame env)))
                              (lambda (v env) (throw v (pop-frame env)))
                              (lambda (env) (next (pop-frame env))))))

;; Interpret a throw statement using the throw continuation.
(define interpret-throw
  (lambda (statement environment throw)
    (throw (eval-expression (get-expr statement) environment) environment)))

;; Create a custom throw continuation that executes catch/finally blocks.
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw next finally-block)
    (if (null? catch-statement)
        (lambda (ex env)
          (interpret-block finally-block env return break continue throw
                           (lambda (env2) (throw ex env2))))
        (lambda (ex env)
          (let* ((new-env (push-frame env))
                 (catch-env (insert (catch-var catch-statement) ex new-env)))
            (interpret-statement-list
             (get-body catch-statement)
             catch-env
             return
             (lambda (env2) (break (pop-frame env2)))
             (lambda (env2) (continue (pop-frame env2)))
             (lambda (v env2) (throw v (pop-frame env2)))
             (lambda (env2)
               (interpret-block finally-block (pop-frame env2) return break continue throw next))))))))

;; Interpret a try-catch-finally block.
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

;; Helper to format a try block into a begin statement.
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

;; Helper to format a finally block into a begin statement.
(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally))
       (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

;; Evaluate an expression or value recursively.
(define eval-expression
  (lambda (expr environment)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr environment))
      (else (eval-operator expr environment)))))

;; Evaluate an operator expression based on its type.
(define eval-operator
  (lambda (expr environment)
    (cond
      ((eq? (operator expr) 'new)
       (make-instance-closure (lookup (operand1 expr) environment)))
      ((eq? (operator expr) 'dot)
       (let* ((inst (eval-expression (operand1 expr) environment))
              (member (operand2 expr)))
         (unless (and (list? inst) (eq? (car inst) 'instance-closure))
           (myerror "dot on non-instance:" inst))
         (let ((fields (list-ref inst 2)))
           (if (hash-has-key? fields member)
               (hash-ref fields member)
               (let* ((class-closure (list-ref inst 1))
                      (methods (list-ref class-closure 4)))
                 (if (hash-has-key? methods member)
                     (list 'bound-method inst (hash-ref methods member))
                     (myerror "Unknown field or method:" member)))))))
      ((eq? (operator expr) 'funcall)
       (let* ((fun (eval-expression (cadr expr) environment))
              (args (map (lambda (arg)
                           (eval-expression arg environment))
                         (cddr expr))))
         (cond
           ((and (list? fun) (eq? (car fun) 'closure))
            (call-function fun args environment))
           ((and (list? fun) (eq? (car fun) 'bound-method))
            (call-method (list-ref fun 2) (list-ref fun 1) args environment))
           (else (myerror "Cannot call non-function:" fun)))))
      ((eq? (operator expr) '!)
       (not (eval-expression (operand1 expr) environment)))
      ((and (eq? (operator expr) '-)
            (= 2 (length expr)))
       (- (eval-expression (operand1 expr) environment)))
      (else
       (eval-binary-op2 expr
                        (eval-expression (operand1 expr) environment)
                        environment)))))

;; Complete binary operation evaluation after first operand is evaluated.
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

;; Check if two values are equal.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))

;; Extract the operator (first element) from an expression.
(define operator car)

;; Extract the first operand (second element) from an expression.
(define operand1 cadr)

;; Extract the second operand (third element) from an expression.
(define operand2 caddr)

;; Extract the third operand (fourth element) from an expression.
(define operand3 cadddr)

;; Check if a second operand exists in a statement.
(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

;; Check if a third operand exists in a statement.
(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

;; Get the statement type.
(define statement-type operator)

;; Get the expression from a return or throw statement.
(define get-expr operand1)

;; Get the variable name from a var declaration.
(define get-declare-var operand1)

;; Get the value (expression) from a var declaration.
(define get-declare-value operand2)

;; Check if a var declaration has an initializer value.
(define exists-declare-value? exists-operand2?)

;; Get the left-hand side of an assignment.
(define get-assign-lhs operand1)

;; Get the right-hand side of an assignment.
(define get-assign-rhs operand2)

;; Get the condition from an if or while statement.
(define get-condition operand1)

;; Get the "then" branch of an if statement.
(define get-then operand2)

;; Get the "else" branch of an if statement.
(define get-else operand3)

;; Get the body from a while loop or a block.
(define get-body operand2)

;; Check if an if statement has an else branch.
(define exists-else? exists-operand3?)

;; Get the try block from a try-catch-finally.
(define get-try operand1)

;; Get the catch block from a try-catch-finally.
(define get-catch operand2)

;; Get the finally block from a try-catch-finally.
(define get-finally operand3)

;; Get the catch variable name from a catch block.
(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))

;; Create a new empty frame (hash table).
(define newframe
  (lambda ()
    (make-hash)))

;; Create a new environment consisting of a single empty frame.
(define newenvironment
  (lambda ()
    (list (newframe))))

;; Push a new frame on top of an environment.
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

;; Pop the top frame off an environment.
(define pop-frame
  (lambda (environment)
    (cdr environment)))

;; Check if a variable exists anywhere in the environment.
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((hash-has-key? (car environment) var) #t)
      (else (exists? var (cdr environment))))))

;; Look up a variable in the environment (converts booleans to language format).
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))

;; Look up a variable, ensuring it has a defined value.
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? value 'novalue)
          (myerror "error: variable without an assigned value:" var)
          value))))

;; Search the environment frames for a variable binding.
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((hash-has-key? (car environment) var) (hash-ref (car environment) var))
      (else (lookup-in-env var (cdr environment))))))

;; Search a single frame for a variable.
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (hash-has-key? frame var))
       (myerror "error: undefined variable" var))
      (else (hash-ref frame var)))))

;; Insert a new binding into the top frame of the environment.
(define insert
  (lambda (var val environment)
    (hash-set! (car environment) var val)
    environment))

;; Update the value of an existing variable in the environment.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (begin (update-existing var val environment)
               environment)
        (myerror "error: variable used but not defined:" var))))

;; Update an existing variable by modifying the correct frame.
(define update-existing
  (lambda (var val environment)
    (if (hash-has-key? (car environment) var)
        (hash-set! (car environment) var val)
        (update-existing var val (cdr environment)))))

;; Create a custom error break using continuations.
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

;; Display an error message and break execution.
(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (acc vals)
                        (if (null? vals)
                            acc
                            (makestr (string-append acc " " (format "~a" (car vals))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

;; Convert a language boolean to a Scheme boolean.
(define language->scheme
  (lambda (v)
    (cond
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

;; Convert a Scheme boolean to a language boolean.
(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))

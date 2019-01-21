(define (eval exp env)
  (cond ((self-evaluating? exp) exp) ;; i.e. a number
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp)) ; it should return the text that was quoted
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

; maps operand to its corresponding values given an evironment
(define (list-of-values exps values)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps env)))))

; evaluates an IF expression in the given environment
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

; it evaluates expressions in the order they occur
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; handles variable assignments
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

; handles variable definitions
(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

; === Specifics of the syntax of the language ===
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

; VARIABLES are represented by symbols
(define (variable? exp) (symbol? exp))

; QUOTATIONS have the form (quote {text-of-quotations}
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

; Identifies lists beginning with a designated symbol
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; ASSIGNMENTS
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;; DEFINITIONS have the form
;;     - (define {var} {value})
;;     - (define ({var} {parameter 1} ... {parameter n})

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caddr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (make-lambda (cdadr exp)     ; parameters
                   (cddr exp))))   ; body


; LAMBDA expressions are lists that begin with the symbol 'lambda
(define (lamba? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

; and a proper lambda constructor
(define (make-lambda parameters body)
  (cons 'lambda (cons  parameters body)))

; CONDITIONALS begin with an IF and have a predicate, a consequent and an optional alternative
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      #f))

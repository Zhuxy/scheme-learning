(library-directories "..")
(import (modules))

(define (eval exp env)
  ((analyze exp) env))

(load "exec-4.22-base.scm")

(define (analyze exp)
  (cond 
    ((self-evaluating? exp) (analyze-self-evaluating exp))
    ((quoted? exp) (analyze-quoted exp))
    ((variable? exp) (analyze-variable exp))
    ((assignment? exp) (analyze-assignment exp))
    ((definition? exp) (analyze-definition exp))
    ((if? exp) (analyze-if exp))
    ((let? exp) (analyze-let (let->combination exp)))
    ((lambda? exp) (analyze-lambda exp))
    ((begin? exp) (analyze-sequence (begin-actions exp)))
    ((cond? exp) (analyze (cond->if exp)))
    ((application? exp) (analyze-application exp))
    (else `analyze (error "Unknown expression type 一- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
    (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      `ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
    (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      `ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
    (cproc (analyze (if-consequent exp)))
    (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (pproc env)
        (cproc env)
        (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
    (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env)
      (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs))
        (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error `analyze-sequence "Empty sequence -- ANALYZE")
      (loop (car procs) (cdr procs)))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
        (fproc env)
        (map (lambda (aproc) (aproc env)) aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
          (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
          ((procedure-body proc)
            (extend-environment (procedure-parameters proc) 
                                args
                                (procedure-environment proc))))
        (else (error `execute-application "Unknown procedure type" proc))))


(displayn "self-evaluating: " (eval `1 the-global-environment))
(displayn "quoted: " (eval `(quote a) the-global-environment))
(displayn "define variable: " (eval `(define a 1) the-global-environment))
(displayn "variable assignment: " (eval `(set! a 2) the-global-environment))
(displayn "if: " (eval `(if (< a 3) (quote good) (quote bad)) the-global-environment))


;4.22
;(let ((var1 exp1) (var2 exp2) ...)
;    body)
;->
;((lambda (var1 var2 ...) body) exp1 exp2 ...)

(define (analyze-let exp)
  (let ((fproc (analyze-lambda (car exp)))
        (aprocs (map analyze (cdr exp))))
      (lambda (env)
        (execute-application
          (fproc env)
          (map (lambda (aproc) (aproc env)) aprocs)))))

(displayn "let: " (eval `(let ((a 1) (b 2)) (+ a b)) the-global-environment))







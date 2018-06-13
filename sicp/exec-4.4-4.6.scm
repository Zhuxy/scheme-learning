(library-directories "..")
(import (modules))

(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((let? exp) (eval (let->combination exp) env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (eval-and (and-seqs exp) env))
        ((or? exp) (eval-or (or-seqs exp) env))
        ((lambda? exp)
            (make-procedure (lambda-parameters exp)
                (lambda-body exp)
                env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
            (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
    (else (error 'exp "Unknown expression type -- EVAL"))))

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
        (else (error 'apply "Unknown procedure type -- APPLY"))))

(define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) evn))))

(define (eval-if exp env)
    (if (true? (eval (if-predicate exp) evn))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
    (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
            (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
    (set-variable-value! 
        (assignment-variable exp)
        (eval (assignment-value exp) env)
        env)
    'ok)

(define (eval-definition exp env)
    (set-variable-value! 
        (definition-variable exp)
        (eval (definition-value exp) env)
        env)
    'ok)

(define (tagged-list? exp tag)
    (if (pair? exp) 
        (eq? (car exp) tag)
        #f))

(define (self-evaluating exp)
    (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
    (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp) (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
    (if (symbol? (cadr exp)) (cadr exp) (caadr exp)))

(define (definition-value exp)
    (if (symbol (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp) (cddr exp))))

;(lambda (x y) (+ x y))
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))

;(if (> x 1) #t #f)
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
    (if (null? (cdddr exp)) #f (cadddr exp)))

(define (make-if predicate consequent alternative)
    (list 'if predicate consequent alternative))

;(begin body1 body1 ...)
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (make-begin seq) (cons 'begin seq))

(define (sequence->exp seq)
    (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

;application
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;(cond
;   (predicate body1 body2 body3 ...)
;   (else body1 body2 body3 ...))
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
    (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
    (if (null? clauses) #f
        (let ((first (car clauses))
                (rest (cdr clauses)))
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error 'cond->if "ELSE clause isn't last -- COND->IF"))
                (make-if (cond-predicate first)
                    (sequence->exp (cond-actions first))
                    (expand-clauses rest))))))

;4.4
;(and (predicate1 exp1) (predicate2 exp2) ...)
(define (and? exp) (tagged-list? exp 'and))

(define (and-seqs exp) (cdr exp))

(define (eval-and seqs env)
    (cond ((null? seqs) #t)
        ((true? (eval (first-exp seqs) env)) (eval-and (rest-exps seqs) env))
        (else #f)))

;(or (predicate1 exp1) (predicate2 exp2) ...)
(define (or? exp) (tagged-list? exp 'or))

(define (or-seqs exp) (cdr exp))

(define (eval-or seqs env)
    (cond ((null? seqs) #f)
        ((true? (eval (first-exp seqs) env)) #t)
        (else (eval-or (rest-exps seqs) env))))

;4.4

;and->if
;(and predicate1 predicate2 predicate3 ...)
;->
;(if predicate1
;   (if predicate12
;       (if predicate13 #t #f) 
;       #f)
;   #f)

(define (and->if exp)
    (expand-and (and-seqs exp)))

(define (expand-and seq)
    (if (null? seq) #t
        (let ((first (first-exp seq))
            (rest (rest-exps seq)))
        (make-if first (expand-and rest) #f))))

(displayn "(and->if '(and (> 2 1) (< 4 1))): " (and->if '(and (> 2 1) (< 4 1))))

;or->if
;(or predicate1 predicate2 predicate3 ...)
;->
;(if predicate1 #t
;   (if predicate2 #t
;       (if predicate3 #t #f)))

(define (or->if exp)
    (expand-or (or-seqs exp)))

(define (expand-or seq)
    (if (null? seq) #f
        (let ((first (first-exp seq))
                (rest (rest-exps seq)))
        (make-if first #t (expand-or rest)))))

(displayn "(or->if '(or (> 1 2) (< 3 4))): " (or->if '(or (> 1 2) (< 3 4))))

;4.5
;(cond (predicate1 body1 body2 ...)
;    (predicate2 => procedure)
;    (predicate3 body3 body4 ...)
;    (else body5 body6 ...))

;(if predicate1 (begin body1 body2)
;    (let ((result predicate2))
;        (if (true? result) (apply procedure result)
;            (if (predicate3) (begin body3 body4)
;                (being body5 body6)))))



(define (expand-clauses clauses)
    (if (null? clauses) #f
        (let ((first (car clauses))
                (rest (cdr clauses)))
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error 'cond->if "ELSE clause isn't last -- COND->IF"))
                (if (cond-recipient-clause? first)
                    ;不考虑副作用的情况下, 执行两次preidiate部分
                    ;(expand-cond-recipient-clause (cond-predicate first) (cond-recipient first))
                    (make-if (cond-predicate first)
                        (list (sequence->exp (cond-recipient first)) (cond-predicate))
                        (expand-clauses rest))
                    (make-if (cond-predicate first)
                        (sequence->exp (cond-actions first))
                        (expand-clauses rest)))))))

(define (cond-recipient-clause? clause)
    (eq? (first-exp (cond-actions clause)) '=>))

(define (cond-recipient clause)
    (first-exp (rest-exps (cond-actions clause))))

;(display (cond-recipient '((> 1 2) => caard)))


;4.6
;(let ((var1 exp1) (var2 exp2) ...)
;    body)
;->
;((lambda (var1 var2 ...) body) exp1 exp2 ...)

(define (let? exp) (tagged-list? exp 'let))

(define (let->combination exp)
    (cons (make-lambda (let-variables exp) (let-body exp)) (let-exps exp)))

(define (let-variables exp)
    (let ((var-definitions (cadr exp)))
        (define (var-it list)
            (if (null? list) '() (cons (caar list) (var-it (cdr list)))))
        (var-it var-definitions)))

;(display (let-variables '(let ((v1 e1) (v2 e2)) body)))

(define (let-exps exp)
    (let ((var-definitions (cadr exp)))
        (define (exp-it list)
            (if (null? list) '() (cons (cadar list) (exp-it (cdr list)))))
        (exp-it var-definitions)))

;(display (let-exps '(let ((v1 e1) (v2 e2)) body)))

(define (let-body exp) (cddr exp))

(displayn "let->combination: " (let->combination '(let ((var1 exp1) (var2 exp2)) body1 body2)))


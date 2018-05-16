(library-directories "..")
(import (modules))

(define (eval exp env)
	(cond ((self-evaluating? exp) exp)
		((variable? exp  (symbol? expe)xp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
 		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
			(make-procedure (lambda-parameters exp)
				(lambda-body exp)
				env))
		((begin? exp) (eval-sequence (begin-actions exp) env))
		((cond? exp) (eval (cond->if exp) env))
		((application? exp)
			(apply (eval (operator exp) env)
				(list-of-values (operands exp) env)))
	(else (error `exp "Unknown expression type -- EVAL"))))

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
		(else (error `apply "Unknown procedure type -- APPLY"))))

(define (list-of-values exps env)
	(if (no-operands? exps)
		`()
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
	`ok)

(define (eval-definition exp env)
	(set-variable-value! 
		(definition-variable exp)
		(eval (definition-value exp) env)
		env)
	`ok)

;4.1
;left order 
(define (list-of-values-l exps env)
	(if (no-operands? exps)
		`()
		(let ((first (eval (first-operand exps) env)))
			(cons first
				(list-of-values (rest-operands exps) evn)))))
;思考
;用begin语句分别对第一个和后续的递归求值
;依赖于lisp/scheme中begin各语句的求值顺序
;begin总是顺序执行? 下一条需要依赖上一条语句执行后变化过的"环境"继续求值
;let语句可以保证赋值部分先求值

;right order
(define (list-of-values-r exps env)
	(if (no-operands? exps)
		`()
		(let ((rest (list-of-values-r (rest-exps exps) env)))
		(cons (eval (first-operand exps) env)
			rest))))

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
	(tagged-list? exp `quote))

(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp) (tagged-list? exp `set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp `define))

(define (definition-variable exp)
	(if (symbol? (cadr exp)) (cadr exp) (caadr exp)))

(define (definition-value exp)
	(if (symbol (cadr exp))
		(caddr exp)
		(make-lambda (cdadr exp) (cddr exp))))

;(lambda (x y) (+ x y))
(define (lambda? exp) (tagged-list? exp `lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
	(cons `lambda (cons parameters body)))

;(if (> x 1) #t #f)
(define (if? exp) (tagged-list? exp `if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
	(if (null? (cdddr exp)) #f (cadddr exp)))

(define (make-if predicate consequent alternative)
	(list `if predicate consequent alternative))

;(begin body1 body1 ...)
(define (begin? exp) (tagged-list? exp `begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (make-begin seq) (cons `begin seq))

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
;	(predicate body1 body2 body3 ...)
;	(else body1 body2 body3 ...))
(define (cond? exp) (tagged-list? exp `cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
	(eq? (cond-predicate clause) `else))

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
					(error `cond->if "ELSE clause isn`t last -- COND->IF"))
				(make-if (cond-predicate first)
					(sequence->exp (cond-actions first))
					(expand-clauses rest))))))











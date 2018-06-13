(library-directories "..")
(import (modules))


(define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
    (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else 
            (eval (first-exp exps) env)
            (eval-sequence (rest-exps exps) env))))

(define (tagged-list? exp tag)
    (if (pair? exp) 
        (eq? (car exp) tag)
        #f))

(define (self-evaluating? exp)
    (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
    (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

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

(define (first-exp seq) 
    (car seq))

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
    (if (null? clauses) 'false
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
    (cond ((null? seqs) 'false)
        ((true? (eval (first-exp seqs) env)) 'true)
        (else (eval-or (rest-exps seqs) env))))

;let
(define (let? exp) (tagged-list? exp 'let))

(define (let->combination exp)
    (if (symbol? (cadr exp))
        (expand-named-let exp)
        (cons (make-lambda (let-variables exp) (let-body exp)) (let-exps exp))))

(define (let-variables exp)
    (let ((var-definitions (cadr exp)))
        (define (var-it list)
            (if (null? list) '() (cons (caar list) (var-it (cdr list)))))
        (var-it var-definitions)))


(define (let-exps exp)
    (let ((var-definitions (cadr exp)))
        (define (exp-it list)
            (if (null? list) '() (cons (cadar list) (exp-it (cdr list)))))
        (exp-it var-definitions)))


(define (let-body exp) (cddr exp))


;let*
(define (let*? exp) (tagged-list? exp 'let*))

(define (make-let var-defs body)
    (cons 'let (cons var-defs body)))

(define (let*->nested-lets exp)
    (let ((var-defs (cadr exp))
            (body (let-body exp)))
        (define (let-it vars body)
            (if (last-exp? vars) (let->combination (make-let vars body))
                (let->combination (make-let (list (car vars)) (list (let-it (cdr vars) body))))))
        (let-it var-defs body)))


;(define (true? x) (not (eq? x #f)))

;(define (false? x) (eq? x #f))

;make-unbound!
(define (make-unbound? exp) (tagged-list? exp 'make-unbound!))

(define (make-unbound-var exp) (cadr exp))

(define (eval-make-unbound var env)
    (define (eval-it var rest)
        (cond ((null? rest) 'not-found)
            ((= (length rest) 1) (set-car! rest '()))
            ((eq? var (caar rest))
                (set-car! rest (cadr rest))
                (set-cdr! rest (cddr rest)))
            (else (eval-it var (cdr rest)))))
    (eval-it var (first-frame env)))


;environment
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures 
    (list 
        (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'display display)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '< <)
        (list '> >)
    )
)

(define primitive-procedure-names (map car primitive-procedures))

(define primitive-procedure-objects
    (map (lambda (proc) (list 'primitive (cadr proc)))
        primitive-procedures))

;env 4.11
;env :=> (frame frame frame ...)
;frame :=> ((var val) (var val) ...)
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values) 
    (if (= (length variables) (length values))
        (if (null? variables) '()
            (cons (cons (car variables) (car values)) 
                (make-frame (cdr variables) (cdr values))))
        (error 'make-frame "to many variables or values")))

(define (add-binding-to-frame! var val frame)
    (set-cdr! frame (cons (cons var val) (cdr frame))))

(define (extend-environment vars vals base-env)
    (if (= (length vars) (length vals))
        (cons (make-frame vars vals) base-env)
        (error 'extend-environment "to many variables or values")))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
    (apply-in-underlying-scheme
        (primitive-implementation proc) args))

(define input-prompt "::: M~Eval input: ")
(define output-prompt "::: M-Eval value: ")

(define (driver-loop)
    (prompt-for-input input-prompt)
    (let ((input (read)))
        (let ((output (eval input the-global-environment)))
            (announce-output output-prompt)
            (user-print output)))
    (driver-loop))

(define (prompt-for-input string)
    (newline)(newline)(display string)(newline))

(define (announce-output string)
    (newline)(display string)(newline))

(define (user-print object)
    (if (compound-procedure? object)
        (display 
            (list 'compound-procedure
                (procedure-parameters object)
                (procedure-body object)
                '<procedure-env>))
        (display object)))

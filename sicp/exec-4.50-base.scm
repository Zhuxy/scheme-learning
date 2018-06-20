(library-directories "..")
(import (modules))

(define (eval exp env)
  ((analyze exp) env))

(define (delayed-eval exp)
  (cons 'delayed exp))

(define (delayed-object? object) (tagged-list? object 'delayed))

(define (force-eval delayed-object env) 
  ;(if (delayed-object? delayed-object)
    (eval (cdr delayed-object) env))
    ;(error 'force-eval "not a delayed eval object" delayed-object)))

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

(define (expand-named-let exp)
  (define var (cadr exp))
  (let ((def (list 'define var (make-lambda (let-variables exp) (let-body exp))))
    (proc (cons var (let-exps exp))))
    (sequence->exp (list def proc))))

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

(define (m-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
      (apply-primitive-procedure procedure arguments))
    ((compound-procedure? procedure)
      (eval-sequence
        (procedure-body procedure)
        (extend-environment
          (procedure-parameters procedure)
          arguments
          (procedure-environment procedure))))
    (else (error 'apply "Unknown procedure type -- APPLY" exp))))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
      (list-of-values (rest-operands exps) env))))

;assignment
(define (assignment? exp) (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (eval-assignment exp env)
  (set-variable-value! 
    (assignment-variable exp)
    (eval (assignment-value exp) env)
    env)
  'ok)

;definition
(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp)) (cadr exp) (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp) (cddr exp))))

(define (eval-definition exp env)
  (define-variable! 
    (definition-variable exp)
    (delayed-eval (definition-value exp))
    env)
  'ok)

;(lambda (x y) (+ x y))
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;procedure
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p) (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

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

(define (lookup-binding-in-frame var frame)
  (if (null? frame) 'not-found
    (if (eq? var (caar frame))
      (car frame)
      (lookup-binding-in-frame var (cdr frame)))))

(define (lookup-binding-in-env var env consumer)
  (if (eq? env the-empty-environment)
    (consumer 'not-found)
    (let ((binding (lookup-binding-in-frame var (first-frame env))))
      (if (eq? 'not-found binding)
        (lookup-binding-in-env var (enclosing-environment env) consumer)
        (consumer binding)))))

(define (lookup-variable-value var env)
  (lookup-binding-in-env var env
    (lambda (r)
      (cond 
        ((eq? r 'not-found)
          (error 'lookup-variable-value "Unbound variable" var))
        ((eq? r '*unassigned*)
          (error 'lookup-variable-value "unassigned variable" var))
        ((delayed-object? (cdr r)) (force-eval (cdr r) env))
        (else 
          (cdr r))))))

(define (set-variable-value! var val env)
  (lookup-binding-in-env var env
    (lambda (r)
      (if (eq? r 'not-found)
        (error 'set-variable-value "Unbound variable")
        (set-cdr! r val)))))

(define (define-variable! var val env)
  (lookup-binding-in-env var env
    (lambda (r)
      (if (eq? r 'not-found)
        (add-binding-to-frame! var val (first-frame env))
        (set-cdr! r val)))))

(define (set-environment)
  (let ((initial-env
      (extend-environment
        primitive-procedure-names
        primitive-procedure-objects
        the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define the-global-environment (set-environment))

(define (scan-out-defines body)
  (define var-defs '())
  (define (trans-define exp)
    (cond 
      ((definition? exp)
        (set! var-defs (cons (list (definition-variable exp) (list 'quote '*unassigned*)) var-defs))
        (set-car! exp 'set!)
        (if (not (symbol? (cadr exp)))
          (set-cdr! exp
            (list (caadr exp)
              (delayed-eval (make-lambda (cdadr exp) (cddr exp)))))
          (set-cdr! exp
            (list (cadr exp)
              (delayed-eval (cddr exp)))))
        exp)
      (else exp)))
  (define new-body (map trans-define body))
  (if (> (length var-defs) 0)
    (list (make-let var-defs new-body))
    body))

(define (letrec? exp) (tagged-list? exp 'letrec))

(define (letrec->let exp)
  (define vars (map car (cadr exp)))
  (define var-definitions (map cadr (cadr exp)))
  (define var-definitions-unassigned (map (lambda (v) (list v '*unassigned)) vars))
  (define vars-temp (map (lambda (v) (gensym (string-append (symbol->string v) "-temp"))) vars))
  (define (merge-map f list1 list2)
    (if (null? list1) '()
      (cons (f (car list1) (car list2)) (merge-map f (cdr list1) (cdr list2)))))
  (define var-temp-definitions 
    (merge-map
      list
      vars-temp
      (map cadr (cadr exp))))
  (define set-causes 
    (merge-map
      (lambda (v1 v2) (list 'set! v1 v2))
      vars vars-temp))
  (make-let
    var-definitions-unassigned
    (list (make-let
      var-temp-definitions
      (append
        set-causes
        (cddr exp))
    ))))

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
    ((amb? exp) (analyze-amb exp))
    ((application? exp) (analyze-application exp))
    (else 'analyze (error "Unknown expression type 一- ANALYZE" exp))))

(define (amb? exp) (tagged-list exp 'amb))

(define (amb-choices exp) (cdr exp))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))
;如果是简单表达式, 则把表达式的值交给成功继续, 同时把fail也继承给成功继续
;succeed和fail就是两个continuration
;(ambeval `(amb 1 2 3) env succeed fail)

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail) 
    (succeed (lookup-variable-value exp env) fail)))
;寻找变量的动作不存在回溯, 找不到报异常也不需要重试

;赋值动作需要记录被覆盖的值, 如果求值成功则在赋值前保留原始值, 在fail continuration中先恢复旧值, 再继续执行fail
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
    (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
        (lambda (value fail2)
          (let ((old-value (lookup-variable-value var env)))
            (set-variable-value! var value env)
            (succeed 'ok
              (lambda ()
                (set-variable-value! var old-value env)
                (fail2)))))
        fail))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
    (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
        (lambda (value fail2)
          (define-variable! var value env)
          (succeed 'ok fail2)
        )
        fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
    (cproc (analyze (if-consequent exp)))
    (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
        (lambda (pred-value fail2)
          (if pred-value
            (cproc env succeed fail2)
            (aproc env succeed fail2)))
        fail))))
;(ambeval `(if (amb choice1 choice2) cproc aproc) env succeed fail)

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
    (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env succeed fail)
      (proc1 env
        (lambda (value fail2)
          (proc2 env succeed fail2))
        fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs))
        (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error 'analyze-sequence "Empty sequence -- ANALYZE")
      (loop (car procs) (cdr procs)))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
        (lambda (proc fail2)
          (get-args aprocs
            env
            (lambda (args fail3)
              (execute-application proc args succeed fail3))
            fail2))
        fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
    (succeed '() fail)
    ((car aprocs) env
      (lambda (arg fail2)
        (get-args (cdr aprocs) env
          (lambda (args fail3)
            (succeed (cons arg args) fail3))
          fail2))
      fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
          (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
          ((procedure-body proc)
            (extend-environment (procedure-parameters proc) 
                                args
                                (procedure-environment proc))))
        (else (error 'execute-application "Unknown procedure type" proc))))

(define (analyze-let exp)
  (let ((fproc (analyze-lambda (car exp)))
        (aprocs (map analyze (cdr exp))))
      (lambda (env)
        (execute-application
          (fproc env)
          (map (lambda (aproc) (aproc env)) aprocs)))))





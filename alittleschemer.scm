(import (modules))

(define rember 
    (lambda (a list) 
        (cond 
            ((null? list) `())
            ((eq? a (car list)) (rember a (cdr list)))
            (else (cons (car list) (rember a (cdr list)))))))


(display (rember `a `(b a a c)))
(newline)

(define firsts
    (lambda (list)
        (cond
            ((null? list) `())
            (else (cons (caar list) (firsts (cdr list)))))))

(display (firsts `((a b c) (e f g) (h j k))))
(newline)

(define insertR
    (lambda (new after list)
        (cond 
            ((null? list) (cons new `()))
            ((eq? after (car list)) 
                (cons (car list) (cons new (cdr list))))
            (else (cons (car list) 
                        (insertR new after (cdr list)))))))

(display (insertR `a `b `(a b c d)))
(newline)

(define tuple?
    (lambda (t)
        (cond 
            ((null? t) #t)
            ((number? (car t)) (tuple? (cdr t)))
            (else #f))))

(display (tuple? `(1 2 3 4 5)))
(newline)

(define addtup
    (lambda (tup)
        (cond 
            ((null? tup) 0)
            (else 
                (+ (car tup) (addtup (cdr tup)))))))

(display (addtup `(1 2 3 4)))
(newline)

(define rember* 
    (lambda (e list)
        (cond 
            ((null? list) `())
            ((atom? (car list))
                (cond 
                    ((eq? e (car list)) (rember* e (cdr list)))
                    (else (cons (car list) (rember* e (cdr list))))))
            (else
                (cons (rember* e (car list)) (rember* e (cdr list)))))))

(displayn "rember* " (rember* `cup `((coffee) cup ((tea) cup) (and (hick)) cup)))

(define insertR*
    (lambda (new after list)
        (cond 
            ((null? list) `())
            ((atom? (car list))
                (cond 
                    ((eq? after (car list)) (cons (car list) (cons new (insertR* new after(cdr list)))))
                    (else 
                        (cons (car list) (insertR* new after (cdr list))))))
            (else (cons
                        (insertR* new after (car list)) (insertR* new after (cdr list)))))))

(displayn "insertR* " (insertR* `a `cup `((coffee) cup ((tea) cup) (and (hick)) cup)))


(define occur*
    (lambda (e list)
        (cond 
            ((null? list) 0)
            ((atom? (car list))
                (cond 
                    ((eq? e (car list)) (+ 1 (occur* e (cdr list))))
                (else (occur* e (cdr list)))))
            (else
                (+ (occur* e (car list)) (occur* e (cdr list)))))))
  
(displayn "occur* " (occur* `cup `((coffee) cup ((tea) cup) (and (hick)) cup)))

(define numbered?
    (lambda (exp)
        ;(begin
        ;    (displayn exp)
        (cond 
            ((atom? exp) (number? exp))
            ((null? (cdr exp)) (number? (car exp)))
            ((eq? (car (cdr exp)) `+) (and (numbered? (car exp)) (or  (number? (cddr exp)) (numbered? (cddr exp)))))
            ((eq? (car (cdr exp)) `-) (and (numbered? (car exp)) (or  (number? (cddr exp)) (numbered? (cddr exp)))))
            ((eq? (car (cdr exp)) `*) (and (numbered? (car exp)) (or  (number? (cddr exp)) (numbered? (cddr exp)))))
            ((eq? (car (cdr exp)) `/) (and (numbered? (car exp)) (or  (number? (cddr exp)) (numbered? (cddr exp)))))
            ((eq? (car (cdr exp)) `^) (and (numbered? (car exp)) (or  (number? (cddr exp)) (numbered? (cddr exp)))))
            (else #f)
        ;)
        )))

(displayn "numbered? " (numbered? `(1 + 1 ^ 1)))

(define set?
    (lambda (s)
        (cond 
            ((null? s) #t)
            ((member (car s) (cdr s)) #f)
            (else (set? (cdr s))))))

(displayn "set? " (set? `(a b c d)))

(define seconds
    (lambda (rel)
        (cond 
            ((null? rel) `())
            (else
                (cons (cdr (car rel)) (seconds (cdr rel)))))))

(displayn "seconds: " (seconds `((a b) (c d) (e f))))

(define insertX
    (lambda (e list pos)
        (cond 
            ((null? list) (cons e `()))
            ((zero? pos) (cons e list))
            (else
                (cons (car list) (insertX e (cdr list) (- pos 1)))))))

(displayn "insertX: " (insertX `a `(1 2 3 4) 3))

(define insertL-f
    (lambda (test?)
        (lambda (new after list)
            (cond 
                ((null? list) (cons new `()))
                ((test? after (car list)) 
                    ;(cons new (cons (car list) (cdr list))))
                    (insertX new list 0))
                (else (cons (car list) 
                            ((insertL-f test?) new after (cdr list))))))))

(define insertR-f
    (lambda (test?)
        (lambda (new after list)
            (cond 
                ((null? list) (cons new `()))
                ((test? after (car list)) 
                    ;(cons (car list) (cons new (cdr list))))
                    (insertX new list 1))
                (else (cons (car list) 
                            ((insertR-f test?) new after (cdr list))))))))

(displayn "insertL-f " ((insertL-f eq?) `a `b `(c d e b)))
(displayn "insertR-f " ((insertR-f eq?) `a `b `(c d e b)))

(define operator?
    (lambda (op exp)
        (cond 
            (((lambda (x) (null? (cdr x))) (cddr exp)) (eq? (cadr exp) op))
            (else (operator? op (cddr exp))))
    ))

(displayn "operator? " (operator? `+ `(1 + 2 + 3)))

(define list-length
    (lambda (list)
        (cond 
            ((null? list) 0)
            (else 
                (+ 1 (list-length (cdr list)))))))

(displayn "list-length: " (list-length `(1 2 3)))

(define sub-list
   (lambda (list start end)
        (cond 
            ((and (eq? start 0) (eq? end 1)) (cons (car list) `()))
            ((not (eq? start 0)) (sub-list (cdr list) (- start 1) (- end 1)))
            (else (cons (car list) (sub-list (cdr list) 0 (- end 1))))
        )))

(displayn "sub-list? " (sub-list `(a b c d e) 4 5))


(define left-exp
    (lambda (exp)
        (cond 
            ((null? exp) `())
            ((atom? exp) exp)
            (else 
                (sub-list exp 0 (- (list-length exp) 2))
            ))))

(define right-exp
    (lambda (exp)
        (cond 
            ((null? exp) `())
            ((atom? exp) exp)
            (else 
                (sub-list exp (- (list-length exp) 1) (list-length exp))
            ))))

(displayn "left-exp:" (left-exp `(1 + 2 + 3)))
(displayn "right-exp:" (right-exp `(1 + 2 + 3)))

(define number-value
    (lambda (exp)
        ;(begin (display exp) (newline)
        (cond 
            ((atom? exp) exp)
            ((null? (cdr exp)) (car exp))
            ((operator? `+ exp) (+ (number-value (left-exp exp)) (number-value (right-exp exp))))
            ((operator? `- exp) (- (number-value (left-exp exp)) (number-value (right-exp exp))))
            ((operator? `x exp) (* (number-value (left-exp exp)) (number-value (right-exp exp))))
            ((operator? `/ exp) (/ (number-value (left-exp exp)) (number-value (right-exp exp))))
            ;((operator? `^ exp) (^ (number-value (left-exp exp)) (number-value (right-exp exp))))
        ;)
        )))

(displayn "number-value: " (number-value `(1 x 2 x 3)))

(define multirember
    (lambda (e list)
        (cond 
            ((null? list) `())
            ((eq? e (car list)) (multirember e (cdr list)))
            (else (cons (car list) (multirember e (cdr list)))))))

(displayn "multirember: " (multirember `a `(a b c d a e f a g)))

(define multirember&co
    (lambda (a lat col) ;最初传入的col更像是continuation, 描述收集到的数据将来怎么处理
        (cond
            ((null? lat) (col `() `()))
            ((eq? (car lat) a) (multirember&co a (cdr lat)
                                    (lambda (newlat seen) (col newlat (cons (car lat) seen))))) ;这里更像是递归收集器, 往两个list里cons元素
            (else (multirember&co a (cdr lat)                                                   ;第一个list是处理过的数据, 第二个list是找到的数据
                        (lambda (newlat seen) (col (cons (car lat) newlat) seen))))))) 

(define a-friend (lambda (x y) (null? y)))

(displayn "multirember&co: " (multirember&co `a `(a) a-friend))

(define multiinsertLR
    (lambda (new oldL oldR lat)
        (cond
            ((null? lat) (quote ()))
            ((eq? (car lat) oldL)
                (cons new
                    (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
            ((eq? (car lat) oldR)
                (cons oldR
                    (cons new (multiinsertLR new oldL oldR (cdr lat)))))
    (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

;核心思想, col需要保存原list里处理后的数据, 和收集到的数据
;
(define multiinsertLR&co
    (lambda (new oldL oldR lat col)
        (cond
            ((null? lat) (col `() 0 0))
            ((eq? (car lat) oldL)
                (multiinsertLR&co new oldL oldR (cdr lat) 
                        (lambda (newl L R) 
                            (col (cons new (cons oldL newl)) (+ L 1) R))))
            ((eq? (car lat) oldR)
                (multiinsertLR&co new oldL oldR (cdr lat)
                                    (lambda (newl L R) 
                                        (col (cons oldR (cons new newl)) L (+ R 1)))))
        (else (multiinsertLR&co new oldL oldR (cdr lat) 
                                    (lambda (newl L R)
                                        (col (cons (car lat) newl) L R)))))))

(displayn "multiinsertLR&co: " 
    (multiinsertLR&co `x `1 `2 `(1 a b 1 2) (lambda (list L R) (cons list (cons L R)))))

(define %
    (lambda (x y)
        (cond 
            ((< x y) x)
            (else (% (- x y) y)))))

(displayn "%? " (% 16 2))

(define even?
    (lambda (x)
        (eq? (% x 2) 0)))

(displayn "even? " (even? 10))

(define evens-only*
    (lambda (list)
        (cond 
            ((null? list) `())
            ((atom? (car list))
                (cond 
                    ((even? (car list)) (evens-only* (cdr list)))
                    (else (cons (car list) (evens-only* (cdr list))))))
            (else (cons 
                (evens-only* (car list)) (evens-only* (cdr list))))
        )))

(displayn "evens-only* " (evens-only* `((1 3 4) 5 (6 7 (9 10)) 11 12)))

(define evens-only*&co
    (lambda (list col)
        (cond 
            ((null? list) (col 1 0))
            ((atom? (car list))
                (cond 
                    ((even? (car list)) (evens-only*&co (cdr list) 
                            (lambda (evens odds)
                                (col (* (car list) evens) odds))))
                    (else (evens-only*&co (cdr list)
                            (lambda (evens odds)
                                (col evens (+ (car list) odds))
                            )))))
            ;(else (cons 
            ;    (evens-only*&co (car list) col)
            ;    (evens-only*&co (cdr list) col)))
            (else 
                (evens-only*&co (car list) 
                    (lambda (evens odds)
                     (cons
                        (* evens (car (evens-only*&co (cdr list) col)))
                        (+ odds (cdr (evens-only*&co (cdr list) col)))
                     )))))))

(define the-last-friend
    (lambda (product sum)
            (cons product sum)))

(displayn "evens-only*&co " (evens-only*&co `(1 (2 3) (5 (6 7))) the-last-friend))

(define pick
    (lambda (x lat)
        (cond
            ((eq? x 1) (car lat))
            (else (pick (- x 1) (cdr lat)))
        )))

(define looking
    (lambda (a lat)
        (keep-looking a (pick 1 lat) lat)))

(define keep-looking
    (lambda (a t lat)
        (cond 
            ((eq? a t) #t)
            ((number? t) (keep-looking a (pick t lat) lat))
            (else #f))))

(displayn "looking: " (looking `a `(6 2 4 a 5 7 3)))



(define first (lambda (p) (car p)))

(define second (lambda (p) (car (cdr p))))

(define third (lambda (p) (car(cdr (cdr p)))))

(define build (lambda (s1 s2) (cons s1 (cons s2 `()))))

(define new-entry
    (lambda (name value old-entry)
        (cond 
            ((null? old-entry) (build (cons name `()) (cons value `())))
            (else (build (cons name (first old-entry)) (cons value (second old-entry))))
    )))

(displayn "new-entry: " (new-entry `a `b `((1 3) (2 4))))

(define lookup-in-entry
    (lambda (name entry entry-f)
        (lookup-in-entry-help name
                (first entry)
                (second entry)
                entry-f)))

(define lookup-in-entry-help
    (lambda (name names values entry-f)
        (cond 
            ((null? names) (entry-f name))
            ((eq? name (car names)) (car values))
            (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

(displayn "lookup-in-entry: "
    (lookup-in-entry `c `((a b c) (1 2 3)) (lambda (x) (cons x `()))))

(define extend-table
    (lambda (entry oldtable)
        (cond 
            ((null? oldtable) (cons entry `()))
            (else (build entry oldtable)))))

(displayn "extend-table: " (extend-table `((a) (1)) `((c d) (2 3))))

(define lookup-in-table
    (lambda (name table table-f)
        (cond 
            ((null? table) (table-f name))
            (else (lookup-in-entry name (car table) 
                (lambda (name) (lookup-in-table name (cdr table) table-f)))))))

(displayn "lookup-in-table: " (lookup-in-table
    `c `(((a d) (1 4)) ((b c) (2 3))) (lambda (x) (cons x `()))))

(define atom-to-action
    (lambda (e)
        (cond
            ((number? e) *const)
            ((eq? e #t) *const)
            ((eq? e #f) *const)
            ((eq? e (quote cons)) *const)
            ((eq? e (quote car)) *const)
            ((eq? e (quote cdr)) *const)
            ((eq? e (quote null?)) *const)
            ((eq? e (quote eq?)) *const)
            ((eq? e (quote atom?)) *const)
            ((eq? e (quote zero?)) *const)
            ((eq? e (quote add1)) *const)
            ((eq? e (quote sub1)) *const)
            ((eq? e (quote number?)) *const)
            (else *identifier))))

(define list-to-action
    (lambda (e)
        (cond
            ((atom? (car e))
                (cond
                    ((eq? (car e) (quote quote)) *quote)
                    ((eq? (car e) (quote lambda)) *lambda)
                    ((eq? (car e) (quote cond)) *cond)
                    (else *application)))
        (else *application))))

(define expression-to-action
    (lambda (e)
        (cond
            ((atom? e) (atom-to-action e))
            (else (list-to-action e)))))

(define value
    (lambda (e)
        (meaning e (quote ()))))

(define meaning
    (lambda (e table)
        ((expression-to-action e) e table)))

(define *const
    (lambda (e table)
        (cond
            ((number? e) e)
            ((eq? e #t) #t)
            ((eq? e #f) #f)
            (else (build (quote primitive) e)))))

(define *quote
    (lambda (e table)
        (text-of e)))

(define text-of
    (lambda (e) (second e)))

(define initial-table
    (lambda (name)
        (car (quote ()))));产生错误

(define *identifier
    (lambda (e table)
        (lookup-in-table e table initial-table)))

(define *lambda
    (lambda (e table)
        (build (quote non-primitive)
            (cons table (cdr e)))))

(define table-of (lambda (closure) (first closure)))

(define formals-of (lambda (closure) (second closure)))

(define body-of (lambda (closure) (third closure)))


(define evcon
    (lambda (lines table)
        (cond
            ((else? (question-of (car lines)))
                    (meaning (answer-of (car lines)) table))
            ((meaning (question-of (car lines)) table)
                    (meaning (answer-of (car lines)) table))
        (else (evcon (cdr lines) table)))))

(define else?
    (lambda (x)
        (cond
            ((atom? x) (eq? x (quote else)))
            (else #f))))

(define question-of
    (lambda (e)
        (first e)))

(define answer-of
    (lambda (e)
        (second e)))

(define *cond
    (lambda (e table)
        (evcon (cond-lines-of e) table)))

(define cond-lines-of
    (lambda (cond-e) (cdr cond-e)))

(define evlis
    (lambda (args table)
        (cond
            ((null? args) (quote ()))
            (else
                (cons (meaning (car args) table)
                (evlis (cdr args) table))))))

(define *application
    (lambda (e table)
        (apply
            (meaning (function-of e) table)
            (evlis (arguments-of e) table))))

(define function-of
    (lambda (e) (car e)))

(define arguments-of
    (lambda (e) (cdr e)))

(define primitive?
    (lambda (l)
        (eq? (first l) (quote primitive))))

(define non-primitive?
    (lambda (l)
        (eq? (first l) (quote non-primitive))))

(define apply
    (lambda (fun vals)
        (cond
            ((primitive? fun)
                (apply-primitive (second fun) vals))
            ((non-primitive? fun)
                (apply-closure (second fun) vals)))))

(define apply-primitive
    (lambda (name vals)
        (cond
            ((eq? name `cons)
                    (cons (first vals) (second vals)))
            ((eq? name (quote car))
                    (car (first vals)))
            ((eq? name (quote cdr))
                    (cdr (first vals)))
            ((eq? name (quote null?))
                    (null? (first vals)))
            ((eq? name (quote eq?))
                    (eq? (first vals) (second vals))))
            ((eq? name (quote atom?))
                    (:atom? (first vals)))
            ((eq? name (quote zero?))
                    (zero? (first vals)))
            ((eq? name (quote add1))
                    (add1 (first vals)))
            ((eq? name (quote sub1))
                    (sub1 (first vals)))
            ((eq? name (quote number?))
                    (number? (first vals)))))

(define :atom?
    (lambda (x)
        (cond
            ((atom? x) #t)
            ((null? x) #t)
            ((eq? (car x) (quote primitive)) #t)
            (else #f))))


(define apply-closure
    (lambda (closure vals)
        (meaning (body-of closure)
            (extend-table
                (new-entry (formals-of closure) vals)
                (table-of closure)))))



































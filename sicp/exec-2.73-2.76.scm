(library-directories "..")
(import (modules))

(define (=number? e v)
    (and (number? e) (= e v)))

(define (veriable? e) 
    (if (and (atom? e) (not (number? e))) #t #f))

(define (same-veriable? v1 v2)
    (if (and (veriable? v1) (veriable? v2) (eq? v1 v2)) #t #f
    ))

(define (sum? e)
    (if (and (pair? e) (eq? `+ (car e))) #t #f))

;取被加数
(define (addend e)
    (if (sum? e) (cadr e) #f))

;(displayn "addend: " (addend `(+ 1 2)))

;加数
(define (augend e)
    (if (sum? e) 
        (if (null? (cdddr e)) (caddr e) (append `(+) (cddr e)))
         #f))

;(displayn "augend: " (augend `(+ 1 2)))
;(displayn "augend: " (augend `(+ 1 2 3)))

(define (make-sum a1 a2)
    (cond 
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        (else (list `+ a1 a2))))

;(displayn "make-sum: " (make-sum `x `1))
;(displayn "make-sum: " (make-sum `x `y))
;(displayn "make-sum: " (make-sum `x `1))
;(displayn "make-sum: " (make-sum `x `0))
;(displayn "make-sum: " (make-sum `0 `x))
;(displayn "make-sum: " (make-sum `3 `1))


(define (product? e)
    (if (and (pair? e) (eq? `* (car e))) #t #f))
;(displayn "product?: " (product? `(* 1 2)))

;被乘数
(define (multiplier e)
    (if (product? e) (cadr e) #f))
;(displayn "multiplier: " (multiplier `(* 1 2)))

;乘数
(define (multiplicand e)
    (if (product? e) 
        (if (null? (cdddr e)) (caddr e) (append `(*) (cddr e)))
         #f))
;(displayn "multiplicand: " (multiplicand `(* 1 2)))
;(displayn "multiplicand: " (multiplicand `(* 1 2 3)))

(define (make-product a1 a2)
    (cond 
        ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list `* a1 a2))))

;(displayn "make-product: " (make-product `a `b))


;2.56
(define (exponentiation? e)
    (and (pair? e) (eq? `** (car e))))

;(displayn "exponentiation?: " (exponentiation? `(** 2 2)))
;(displayn "exponentiation?: " (exponentiation? `(* 2 2)))

(define (base e)
    (if (exponentiation? e) (cadr e) #f))

(define (exponent e)
    (if (exponentiation? e) (caddr e) #f))

;(displayn "base: " (base `(** 2 3)))
;(displayn "exponent: " (exponent `(** 2 3)))

(define (make-exponentiation b e)
    (cond ((=number? b 1) 1)
        ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list `** b e))))

;(displayn "make-exponentiation: " (make-exponentiation `1 `2))
;(displayn "make-exponentiation: " (make-exponentiation `100 `0))
;(displayn "make-exponentiation: " (make-exponentiation `a `2))
;(displayn "make-exponentiation: " (make-exponentiation `0 `b))
;(displayn "make-exponentiation: " (make-exponentiation `a `b))

(define (make-minus e1 e2)
    (if (=number? e2 0) e1 (list `- e1 e2)))

;deruv for 2.5X
;(define (deriv e var)
;    (cond ((number? e) 0)
;        ((veriable? e) (if (same-veriable? e var) 1 0))
;        ((sum? e) 
;            (make-sum (deriv (addend e) var) (deriv (augend e) var)))
;        ((product? e)
;            (make-sum 
;                (make-product (multiplier e) (deriv (multiplicand e) var)) 
;                (make-product (multiplicand e) (deriv (multiplier e) var))))
;        ((exponentiation? e)
;            (make-product
;                (make-product 
;                    (exponent e) 
;                    (make-exponentiation (base e) (make-minus (exponent e) 1)))
;                (deriv (base e) var)))
;        (else #f)
;    ))

;(displayn "deriv: " (deriv `(+ x 1) `x))
;(displayn "deriv: " (deriv `(* x 1) `x))
;(displayn "deriv: " (deriv `(* x y) `x))
;(displayn "deriv: " (deriv `(* (* x y) (+ x 3)) `x))
;(displayn "deriv: " (deriv `(** x n) `x))
;(displayn "deriv: " (deriv `(* (* x y) (+ x 3)) `x))
;(displayn "deriv: " (deriv `(* x y (+ x 3)) `x));

;2.73 a
;根据"求导"动作及操作雷系op, 查找对应的表找到处理运算过程
;Number?和same-variable?对应的只有操作数没有操作类型

;2.73 b c
(define table `())
(define (put op type item)
    (set! table (cons (list op type item) table)))
(define (get op type)
    (define (get-l op type list)
        (if (null? list) (error "no op and type found in table" (list op type))
            (let ((head (car list)))
                (if (and (equal? op (car head)) (equal? type (cadr head))) (caddr head)
                    (get-l op type (cdr list))))))
    (get-l op type table))

(put `test `test (lambda () `haha))
(displayn "test put and get: " ((get `test `test)))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv exp var)
    (cond ((number? exp) 0)
        ((veriable? exp) (if (same-veriable? exp var) 1 0))
        (else ((get `deriv (operator exp)) (operands exp) var))))

;安装和式的求导过程
(put `deriv `+ (lambda (operands var)
    (make-sum (deriv (car operands) var) (deriv (cadr operands) var))))

;安装乘式的求导过程
(put `deriv `* (lambda (operands var)
    (make-sum 
        (make-product (car operands) (deriv (cadr operands) var)) 
        (make-product (cadr operands) (deriv (car operands) var)))))

;安装乘幂的求导过程
(put `deriv `** (lambda (operands var)
    (make-product
        (make-product 
            (cadr operands) 
            (make-exponentiation (car operands) (make-minus (cadr operands) 1)))
        (deriv (car operands) var))))

(displayn "deriv: " (deriv `(+ x 1) `x))
(displayn "deriv: " (deriv `(* x 1) `x))
(displayn "deriv: " (deriv `(* x y) `x))
(displayn "deriv: " (deriv `(* (* x y) (+ x 3)) `x))
(displayn "deriv: " (deriv `(* (* x y) (+ x 3)) `x))
(displayn "deriv: " (deriv `(** x n) `x))


;2.74
(define (get-record name branch)
    (let ((branch-name (get-branch-name branch))
            (have-name (get `have-name branch-name table))
            (get-salary (get `get-salary branch-name table))
            (get-address (get `get-address branch-name table)))
        (if (not (have-name name branch)) null 
            (list name
                (get-salary name branch)
                (get-address name branch)))))

;2.74

;2.76
;显示分派 - 有新的类型新的操作都要修改分派代码
;数据导向 - 通过注册操作的方式扩展, 适合经常需要添加操作的系统 
;消息传递 - 通过封装操作加消息传递方式, 适合经常需要扩展类型的系统, 有点面向对象的雏形?







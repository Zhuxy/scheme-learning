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
    (if (and (pair? e) (eq? '+ (car e))) #t #f))

;取被加数
(define (addend e)
    (if (sum? e) (cadr e) #f))

(displayn "addend: " (addend '(+ 1 2)))

;加数
(define (augend e)
    (if (sum? e) 
        (if (null? (cdddr e)) (caddr e) (append '(+) (cddr e)))
         #f))

(displayn "augend: " (augend '(+ 1 2)))
(displayn "augend: " (augend '(+ 1 2 3)))

(define (make-sum a1 a2)
    (cond 
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        (else (list '+ a1 a2))))

(displayn "make-sum: " (make-sum 'x '1))
(displayn "make-sum: " (make-sum 'x 'y))
(displayn "make-sum: " (make-sum 'x '1))
(displayn "make-sum: " (make-sum 'x '0))
(displayn "make-sum: " (make-sum '0 'x))
(displayn "make-sum: " (make-sum '3 '1))


(define (product? e)
    (if (and (pair? e) (eq? '* (car e))) #t #f))
(displayn "product?: " (product? '(* 1 2)))

;被乘数
(define (multiplier e)
    (if (product? e) (cadr e) #f))
(displayn "multiplier: " (multiplier '(* 1 2)))

;乘数
(define (multiplicand e)
    (if (product? e) 
        (if (null? (cdddr e)) (caddr e) (append '(*) (cddr e)))
         #f))
(displayn "multiplicand: " (multiplicand '(* 1 2)))
(displayn "multiplicand: " (multiplicand '(* 1 2 3)))

(define (make-product a1 a2)
    (cond 
        ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list '* a1 a2))))

(displayn "make-product: " (make-product 'a 'b))


;2.56
(define (exponentiation? e)
    (and (pair? e) (eq? '** (car e))))

(displayn "exponentiation?: " (exponentiation? '(** 2 2)))
(displayn "exponentiation?: " (exponentiation? '(* 2 2)))

(define (base e)
    (if (exponentiation? e) (cadr e) #f))

(define (exponent e)
    (if (exponentiation? e) (caddr e) #f))

(displayn "base: " (base '(** 2 3)))
(displayn "exponent: " (exponent '(** 2 3)))

(define (make-exponentiation b e)
    (cond ((=number? b 1) 1)
        ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))

(displayn "make-exponentiation: " (make-exponentiation '1 '2))
(displayn "make-exponentiation: " (make-exponentiation '100 '0))
(displayn "make-exponentiation: " (make-exponentiation 'a '2))
(displayn "make-exponentiation: " (make-exponentiation '0 'b))
(displayn "make-exponentiation: " (make-exponentiation 'a 'b))

(define (make-minus e1 e2)
    (if (=number? e2 0) e1 (list '- e1 e2)))

;deruv for 2.5X
(define (deriv e var)
    (cond ((number? e) 0)
        ((veriable? e) (if (same-veriable? e var) 1 0))
        ((sum? e) 
            (make-sum (deriv (addend e) var) (deriv (augend e) var)))
        ((product? e)
            (make-sum 
                (make-product (multiplier e) (deriv (multiplicand e) var)) 
                (make-product (multiplicand e) (deriv (multiplier e) var))))
        ((exponentiation? e)
            (make-product
                (make-product 
                    (exponent e) 
                    (make-exponentiation (base e) (make-minus (exponent e) 1)))
                (deriv (base e) var)))
        (else #f)
    ))

(displayn "deriv: " (deriv '(+ x 1) 'x))
(displayn "deriv: " (deriv '(* x 1) 'x))
(displayn "deriv: " (deriv '(* x y) 'x))
(displayn "deriv: " (deriv '(* (* x y) (+ x 3)) 'x))
(displayn "deriv: " (deriv '(** x n) 'x))
(displayn "deriv: " (deriv '(* (* x y) (+ x 3)) 'x))
(displayn "deriv: " (deriv '(* x y (+ x 3)) 'x));2.57







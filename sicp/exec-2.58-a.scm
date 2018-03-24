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
    (if (and (pair? e) (eq? `+ (cadr e))) #t #f))
    
(displayn "sum?: " (sum? `(1 + 2)))

;取被加数
(define (addend e)
    (if (sum? e) (car e) #f))

(displayn "addend: " (addend `(+ 1 2)))

;加数
(define (augend e)
    (if (sum? e) (caddr e) #f))

(displayn "augend: " (augend `(+ 1 2)))

(define (make-sum a1 a2)
    (cond 
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        (else (list a1 `+ a2))))

(displayn "make-sum: " (make-sum `x `1))
(displayn "make-sum: " (make-sum `x `y))
(displayn "make-sum: " (make-sum `x `1))
(displayn "make-sum: " (make-sum `x `0))
(displayn "make-sum: " (make-sum `0 `x))
(displayn "make-sum: " (make-sum `3 `1))


(define (product? e)
    (if (and (pair? e) (eq? `* (cadr e))) #t #f))
(displayn "product?: " (product? `(1 * 2)))

;被乘数
(define (multiplier e)
    (if (product? e) (car e) #f))
(displayn "multiplier: " (multiplier `(1 * 2)))

;乘数
(define (multiplicand e)
    (if (product? e) (caddr e) #f))
(displayn "multiplicand: " (multiplicand `(1 * 2)))

(define (make-product a1 a2)
    (cond 
        ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list a1 `* a2))))

(displayn "make-product: " (make-product `a `b))

(define (deriv e var)
    (cond ((number? e) 0)
        ((veriable? e) (if (same-veriable? e var) 1 0))
        ((sum? e) 
            (make-sum (deriv (addend e) var) (deriv (augend e) var)))
        ((product? e)
            (make-sum 
                (make-product (multiplier e) (deriav (multiplicand e) var)) 
                (make-product (multiplicand e) (deriv (multiplier e) var))))
        (else #f)
    ))

(displayn "deriv: " (deriv `(x + 1) `x))
(displayn "deriv: "  (deriv `(x * 1) `x))
(displayn "deriv: "  (deriv `(x * y) `x))
(displayn "deriv: "  (deriv `((x * y) * (x * 3)) `x))
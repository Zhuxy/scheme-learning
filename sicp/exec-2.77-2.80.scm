(library-directories "..")
(import (modules))

;2.77
(define table `())

(define (put op type item)
    (set! table (cons (list op type item) table)))

(trace-define (get op type)
    (define (get-l op type list)
        (if (null? list) #f
            (let ((head (car list)))
                (if (and (equal? op (car head)) (equal? type (cadr head))) (caddr head)
                    (get-l op type (cdr list))))))
    (get-l op type table))

(define (attach-tag type-tag contents)
    (if (number? contents) contents
        (cons type-tag contents)))

(define (type-tag datum)
    (cond ((pair? datum) (car datum))
        ((number? datum) `scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(trace-define (contents datum)
    (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(trace-define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        ;(display op)(newline)
        ;(display type-tags)(newline)
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (error "no method for these types -- APPLY-GENERIC" (list op type-tags))))))

(define (square x) (* x x))


(define (add x y) (apply-generic `add x y))
(define (sub x y) (apply-generic `sub x y))
(define (mul x y) (apply-generic `mul x y))
(define (div x y) (apply-generic `div x y))

(define (install-scheme-number-package) 
    (define (tag x) (attach-tag `scheme-number x))
    (put `add `(scheme-number scheme-number)
        (lambda (x y) (tag (+ x y))))
    (put `sub `(scheme-number scheme-number)
        (lambda (x y) (tag (- x y))))
    (put `mul `(scheme-number scheme-number)
        (lambda (x y) (tag (* x y))))
    (put `div `(scheme-number scheme-number)
        (lambda (x y) (tag (/ x y))))
    (put `equ? `(scheme-number scheme-number) =)
    (put `=zero? `(scheme-number) (lambda (x) (= x 0)))
    (put `make `scheme-number 
        (lambda (x) (tag x)))
    `done)
(install-scheme-number-package)

(displayn "add scheme number:" (add (cons `scheme-number 1) (cons `scheme-number 2)))

(define (install-rational-package)
    (define (numer x) (car x)) 
    (define (denom x) (cdr x))
    (define (make-rat n d)
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))))
    (define (add-rat x y)
        (make-rat (+ (* (numer x) (denom y)) 
                    (* (numer y) (denom x)))
                (* (denom x) (denom y))))
    (define (sub-rat x y)
        (make-rat (- (* (numer x) (denom y))
                    (* (numer y) (denom x)))
                (* (denom x) (denom y))))
    (define (mul-rat x y)
        (make-rat (* (numer x) (number y)) 
                (* (denom x) (numer y))))
    (define (tag x) (attach-tag `rational x)) 
    (put `add `(rational rational)
        (lambda (x y) (tag (add-rat x y))))
    (put `sub `(rational rational) 
        (lambda (x y) (tag (sub-rat x y))))
    (put `mul `(rational rational) 
        (lambda (x y) (tag (mul-rat x y))))
    (put `div `(rational rational) 
        (lambda (x y) (tag (div-rat x y))))
    (put `equ? `(rational rational)
        (lambda (x y) (equal? x y)))
    (put `=zero? `(rational) (lambda (x) (= (numer x) 0)))
    (put `make `rational 
        (lambda (n d) (tag (make-rat n d))))
    `done)
(install-rational-package)

(define (make-rational n d)
    ((get `make `rational) n d))

(displayn "add rational: " (add (make-rational 1 2) (make-rational 1 3)))

(define (install-rectangular-package)
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (make-from-real-imag x y) (cons x y))
    (define (magnitude z)
        (sqrt (+ (square (real-part z))
                (square (imag-part z)))))
    (define (angle z)
        (atan (imag-part z) (real-part z)))
    (define (make-from-mag-ang r a)
        (cons (* r (cos a)) (* r (sin a))))
    (define (tag x) (attach-tag `rectangular x))
    (put `real-part `(rectangular) real-part)
    (put `imag-part `(rectangular) imag-part)
    (put `magnitude `(rectangular) magnitude)
    (put `angle `(rectangular) angle)
    (put `make-from-real-imag `rectangular
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put `make-from-mag-ang `rectangular
        (lambda (r a) (tag (make-from-mag-ang r a))))
    `done)
(install-rectangular-package)

(define (install-polar-package)
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (make-from-mag-ang r a) (cons r a))
    (define (real-part z)
        (* (magnitude z) (cos (angle z))))
    (define (imag-part z)
        (* (magnitude z) (sin (angle z))))
    (define (make-from-real-imag x y)
        (cons (sqrt (+ (square x) (square y)))
                (atan y x)))
    (define (tag x) (attach-tag `polar x))
    (put `real-part `(polar) real-part)
    (put `imag-part `(polar) imag-part)
    (put `magnitude `(polar) magnitude)
    (put `angle `(polar) angle)
    (put `make-from-real-imag `polar
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put `make-from-mag-ang `polar
        (lambda (r a) (tag (make-from-mag-ang r a))))
    `done)
(install-polar-package)


(define (install-complex-package)
    (define (make-from-real-imag x y)
        ((get `make-from-real-imag `rectangular) x y))
    (define (make-from-mag-ang r a)
        ((get `make-from-mag-ang `polar) r a))
    (define (real-part z)
        (apply-generic `real-part z))
    (define (imag-part z)
        (apply-generic `imag-part z))
    (define (magnitude z)
        (apply-generic `magnitude z))
    (define (angle z)
        (apply-generic `angle z))
    (define (add-complex zl z2)
        (make-from-real-imag (+ (real-part z1) (real-part z2))
                            (+ (imag-part z1) (imag-part z2))))
    (define (sub-complex z1 z2)
        (make-from-real-imag (- (real-part z1) (real-part z2))
                            (- (imag-part z1) (imag-part z2))))
    (define (mul-complex z1 z2)
        (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                            (+ (angle z1) (angle z2))))
    (define (div-complex z1 z2)
        (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                            (- (angle z1) (angle z2))))
    (define (tag z) (attach-tag `complex z))
    (put `add `(complex complex)
        (lambda (zl z2) (tag (add-complex zl z2))))
    (put `sub `(complex complex)
        (lambda (zl z2) (tag (sub-complex zl z2))))
    (put `mul `(complex complex) 
        (lambda (zl z2) (tag (mul-complex zl z2))))
    (put `div `(complex complex)
        (lambda (zl z2) (tag (div-complex zi z2))))
    (put `make-from-real-imag `complex 
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put `make-from-mag-ang `complex 
        (lambda (r a) (tag (make-from-mag-ang r a))))
    (put `real-part `(complex) real-part)
    (put `imag-part `(complex) imag-part)
    (put `magnitude `(complex) magnitude)
    (put `angle `(complex) angle)
    `done)
(install-complex-package)

(define (make-complex-from-real-imag x y)
    ((get `make-from-real-imag `complex) x y))
(define (make-complex-from-mag-ang r a)
    ((get `make-from-mag-ang `complex) r a))

;2.77
(displayn "magnitude complex: " (apply-generic `magnitude (make-complex-from-real-imag 3 4)))

;2.78
(displayn "scheme-number add: " (apply-generic `add 1 2))

;2.79
(displayn "scheme-number equ?: " (apply-generic `equ? 1 1))
(displayn "rational equ?: " (apply-generic `equ? (make-rational 1 2) (make-rational 1 2)))
;不同表述系统的复数的相等判断, 会存在精度的误差

;2.80









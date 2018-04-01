(library-directories "..")
(import (modules))

;2.77
(define table `())

(define (put op type item)
    (set! table (cons (list op type item) table)))

(define (get op type)
    (define (get-l op type list)
        (if (null? list) (error "no op and type found in table" (list op type))
            (let ((head (car list)))
                (if (and (eq? op (car head)) (eq? type (cadr head))) (caddr head)
                    (get-l op type (cdr list))))))
    (get-l op type table))

(define (attach—tag type-tag contents)
    (cons type-tag contents))

(define (type-tag datum)
    (if (pair? datum)
        (car datum)
    (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply—generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (error "no method for these types -- APPLY-GENERIC" (list op type-tags))))))


(define (add x y) (apply—generic add x y))
(define (sub x y) (apply—generic sub x y))
(define (mul x y) (apply—generic mul x y))
(define (div x y) (apply—generic div x y))

(define (install-scheme-number-package) 
    (define (tag x) (attach—tag `scheme—number x))
    (put `add `(scheme—number scheme—number)
        (lambda (x y) (tag (+ x y))))
    (put `sub `(scheme—number scheme—number) 
        (lambda (x y) (tag (— x y))))
    (put `mul `(scheme—number scheme—number)
        (lambda (x y) (tag (* x y))))
    (put `div `(scheme—number scheme—number) 
        (lambda (x y) (tag (/ x y)))) 
    (put `make `scheme—number 
        (lambda (x) (tag x)))
    `done)

(define (install—ration•al—package) 
    (define (numer x) (car x)) 
    (define (denom x) (cdr x))
    (define (make—rat n d)
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))))
    (define (add—rat x y)
        (make—rat (+ (* (numer x) (denom Y)) 
                    (* (numer y) (denom x)))
                (* (denom x) (denom y))))
    (define (sub—rat x y)
        (make—rat (- (* (numer x) (denom y))
                    (* (numer y) (denom x)))
                (* (denom x) (denom y))))
    (define (mul—rat x y)
        (make—rat (* (numer x) (number y)) 
                (* (denom x) (numer y))))
    (define (tag x) (attach—tag `rational x)) 
    (put `add `(rational rational)
        (lambda (x y) (tag (add—rat x y))))
    (put `sub `(rational rational) 
        (lambda (x y) (tag (sub—rat x y))))
    (put `mul `(rational rational) 
        (lambda (x y) (tag (mul—rat x y))))
    (put `div `(rational rational) 
        (lambda (x y) (tag (div—rat x y))))
    (put `make `rational 
        (lambda (n d) (tag (make—rat n d))))
    `done)

(define (make—rational n d)
    ((get `make `rational) n d))

(define (install-complex-package)
    (define (make-from-real-imag x y)
        ((get `make-from-real-imag `rectangular) x y))
    (define (make—from—mag—ang r a)
        ((get `make—from—mag—ang `polar) r a)) 
    (define (add—complex zl z2)
        (make-from-real-imag (+ (real—part z1) (real—part z2))
                            (+ (imag-part z1) (imag-part z2))))
    (define (sub-complex z1 z2)
        (make-from-real-imag (- (real—part z1) (real—part z2))
                            (- (imag-part z1) (imag-part z2))))
    (define (mul-complex z1 z2)
        (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                            (+ (angle z1) (angle z2))))
    (define (div-complex z1 z2)
        (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                            (- (angle z1) (angle z2))))
    (define (tag z) (attach—tag `complex z))
    (put `add `(complex complex)
        (lambda (zl z2) (tag (add—complex zl z2))))
    (put `sub `(complex complex)
        (lambda (zl z2) (tag (sub—complex zl z2))))
    (put `mul `(complex complex) 
        (lambda (zl z2) (tag (mul—complex zl z2))))
    (put `div `(complex complex)
        (lambda (zl z2) (tag (div—complex zi z2))))
    (put `make-from-real-imag `complex 
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put `make—from—mag—ang `complex 
        (lambda (r a) (tag (make-from-mag-ang r a))))
    `done)

(define (make-complex-from-real-imag x y)
    ((get `make-from-real-imag `complex) x y))
(define (make-complex-from-mag-ang r a)
    ((get `make—from—mag—ang `complex) r a))









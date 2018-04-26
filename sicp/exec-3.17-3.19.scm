(library-directories "..")
(import (modules))

;3.17
(define (count-pairs lst)
    (define counted `())
    (define (not-counted x counted)
        (cond ((null? counted) #t)
            ((eq? x (car counted)) #f)
            (else (not-counted x (cdr counted)))))
    (define (count-pairs-it x cnt)
        (cond ((null? x) cnt)
            ((and (pair? x) (not-counted x counted))
                (begin
                    (set! counted (cons x counted))
                    (+ cnt 1 (count-pairs-it (car x) 0) (count-pairs-it (cdr x) 0))))
            (else cnt)))
    (count-pairs-it lst 0))

(define x (cons `a `b))
(define y (cons x x))

(displayn "count-pairs: " (count-pairs (list y y y)))

;3.18
(define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))))

(define (make-cycle x)
    (set-cdr! (last-pair x) x)
    x)

(define (check-cycle lst)
    (define counted `())
    (define (not-counted x counted)
        (cond ((null? counted) #t)
            ((eq? x (car counted)) #f)
            (else (not-counted x (cdr counted)))))
    (define (check-cycle-it lst)
        (cond ((null? lst) #f)
            ((not-counted lst counted)
                (begin
                    (set! counted (cons lst counted))
                    (check-cycle-it (cdr lst))))
            (else #t)))
    (check-cycle-it lst))

(displayn "check-cycle: " (check-cycle (make-cycle `(a b c d e f g))))
(displayn "check-cycle: " (check-cycle `(a b c d a)))

;3.19


















(library-directories "..")
(import (modules))

;3.1
(define (make-accumulator init)
    (lambda (x) (begin (set! init (+ init x)) init)))

(define acc (make-accumulator 0))

(display (acc 5))(newline)
(display (acc 5))(newline)
(display (acc 5))(newline)


;3.2
(define (make-monitored f)
    (let ((count 0))
        (lambda (a) 
            (cond ((equal? a 'how-many-calls?) count)
                ((equal? a 'reset-count) (begin (set! count 0) 'reseted!))
                (else (begin 
                        (set! count (+ count 1))
                        (f a)))))))

(define s (make-monitored sqrt))

(display (s 100))(newline)
(display (s 144))(newline)
(display (s 'how-many-calls?))(newline)

;3.3 3.4
(define (make-account balance password)
    (define (withdraw amount)
        (if (< (- balance amount) 0) "Insufficient funds"
            (begin (set! balance (- balance amount)) balance)))
    (define (deposit amount) 
        (begin (set! balance (+ balance amount)) balance))
    (let ((max-wrong 0))
        (lambda (p m amount)
            (if (not (equal? p password)) 
                (begin 
                    (set! max-wrong (+ max-wrong 1))
                    (if (>= max-wrong 7) "call-the-police" "Incorrect password"))
                (cond ((equal? m 'withdraw) (withdraw amount))
                    ((equal? m 'deposit) (deposit amount))
                    (else "wrong command"))))))

(define acc (make-account 100 'pass1234))
(displayn "withdraw: " (acc 'pass1234 'withdraw 20))
(displayn "deposit: " (acc 'wrongpass 'deposit 100))
;(displayn "deposit: " (acc 'wrongpass 'deposit 100))
;(displayn "deposit: " (acc 'wrongpass 'deposit 100))
;(displayn "deposit: " (acc 'wrongpass 'deposit 100))
;(displayn "deposit: " (acc 'wrongpass 'deposit 100))
;(displayn "deposit: " (acc 'wrongpass 'deposit 100))
;(displayn "deposit: " (acc 'wrongpass 'deposit 100))
;(displayn "deposit: " (acc 'wrongpass 'deposit 100))

;3.7
(define (make-joint acc old-password new-password)
    (lambda (p m amount)
        (if (equal? p new-password) (acc old-password m amount)
            (error "wrong password" '()))))

(define acc-new (make-joint acc 'pass1234 'newpassword))
(displayn "deposit: " (acc-new 'newpassword 'deposit 100))

;3.8
(define (make-f)
    (let ((t -1))
        (lambda (x) 
            (begin (set! t (+ t 1))
                (if (= t 0) x 0)))))

(define f (make-f))
(display (+ (f 1) (f 0)))














(library-directories "..")
(import (modules))


(define (make-wire)
    (let ((signal-value 0) (action-procedures '()))
        (define (set-my-signal! new-value)
            (if (not (= signal-value new-value))
                (begin
                    (set! signal-value new-value)
                    (call-each action-procedures))
                'done))
        (define (accept-action-procedure! proc)
            (set! action-procedures (cons proc action-procedures))
            (proc))
        (define (dispatch m)
            (cond 
                ((eq? m 'get-signal) signal-value)
                ((eq? m 'set-signal) set-my-signal!)
                ((eq? m 'add-action!) accept-action-procedure!)
                (else (error 'make-wire "Unknow operation" m))))
        dispatch))
        
(define (call-each list)
    (cond
        ((null? list) 'done)
        (else
            ((car list))
            (call-each (cdr list)))))
            
(define (get-signal wire)
    (wire 'get-signal))
    
(define (set-signal! wire)
    (wire 'set-signal!))
    
(define (add-action! wire action-procedure)
    ((wire 'add-action!) aciton-procedure))

(define (inverter input output)
    (define (invert-input)
        (let ((new-value  (logical-not (get-signal input))))
            (after-delay inverter-delay
                (lambda () (set-signal! output new-value)))))
    (add-action! input invert-input)
    'ok)

(define (logical-not s)
    (cond ((= S 0) 1)
            ((= s 1) 0)
    (else (error 'logical-not "Invalid signal" 8))))

(define (logical-or a1 a2)
    (if (and (= a1 0) (= a2 0)) 0 1))

(define (logical-and a1 a2)
    (if (and (= a1 1) (= a2 1)) 1 0))
    
(define (and-gate a1 a2 output)
    (define (and-gate-action)
        (let ((value (logical-and (get-signal a1) (get-signal a2))))
            (after-delay and-gate-delay
                (lambda () (set-signal! output value)))))
    (add-action! a1 and-gate-action)
    (add-action! a2 and-gate-action))    

;3.28
(define (or-gate a1 a2 output)
    (define (or-gate-action)
        (let ((value (logical-or (get-signal a1) (get-signal a2))))
            (after-delay or-gate-delay
                (lambda () (set-signal! output value)))))
    (add-action! a1 or-gate-action)
    (add-action! a2 or-gate-action))

;3.29
(define (or-gate-1 a b output)
    (let ((a1 (make-wire))
            (a2 (make-wire))
            (o1 (make-wire))
            (o2 (make-wire)) 
            (s (make-wire)))
        (and-gate a b a1)
        (and-gate a b a2)
        (or-gate a1 o1)
        (or-gate a2 o2)
        (and-gate o1 o2 s)
        (or-gate s output)
        'ok
    ))
;当前解法是(or a b) = (not (and (not (and a b)) (not (and a b))))
;延时为 delay-and * 2 + delay-or * 2
;另一种解法 (or a b) = (not (and (not a) (not b)))


;3.30
(define (half-adder a b s c)
    (let ((d (make-wire)) (e (make-wire)))
        (or-gate a b d)
        (and-gate a b c)
        (inverter c e)
        (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
    (let ((s (make-wire))
            (c1 (make-wire))
            (c2 (make-wire)))
        (half-adder b c-in s c1)
        (half-adder a s sum c2)
        (or-gate c1 c2 c-out)
        'ok))

(define (ripple-carry-adder list-a list-b list-s c)
    (define (iterator la lb ls c-out)
        (cond ((null? la) 'ok)
            (else
                (let ((temp-c-in (make-wire)));最后一个c-in只能是0, 这里传0? 没讲怎么创建始终为0的wire
                    (full-adder
                        (car la)
                        (car lb)
                        temp-c-in
                        (car ls)
                        c-out)
                    (set! last-c-out c-in)
                    (iterator (cdr la) (cdr lb) (cdr ls) temp-c-in)
                ))))
    (iterator list-a list-b list-s c))
;delay-half-adder = (+ (max (+ delay-and-gate delay-inverter) delay-or-gate) delay-and-gate)
;delay-full-adder = (+ (* delay-half-adder 2) delay-or-gate)
;delay-ripple-carry-adder = (* n delay-full-adder)







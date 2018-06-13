(library-directories "..")
(import (modules))

;stream
(define the-empty-stream '(()))

(define (stream-null? s) (equal? s the-empty-stream))

(define (stream-ref s n)
    (if (= n 0) (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
    (if (stream-null? s) the-empty-stream
        (cons-stream (proc (stream-car s))
            (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
    (if (stream-null? s) 'done
        (begin (proc (stream-car s))
            (stream-for-each proc (stream-cdr s)))))

(define (memo-proc proc)
    (let ((already-run? #f) (result #f))
        (lambda ()
            (if (not already-run?)
                (begin 
                    (set! result (proc))
                    (set! already-run? #t)
                    result)
                result))))

(define (delay exp) (lambda () exp))

(define (force delayed-object) (delayed-object))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

;必须使用macro来实现, 因为直接cons a (delay d), 会优先对第二个参数b求值再调用delay
(define-syntax cons-stream
    (syntax-rules ()
        ((cons-stream a b)
            (cons a ((lambda () 
                (let ((already-run? #f) (result #f))    ;在匿名函数作用域内定义内部变量
                    (lambda ()                          ;返回使用这些内部变量的函数(闭包)
                        (if (not already-run?)
                            (begin 
                                (set! result ((lambda () b)))
                                (set! already-run? #t)
                                result)
                            result))))))
        )))

;(define (cons-stream a b) (cons a (delay b)))

(define (stream-for-each proc s)
    (if (stream-null? s) 'done
        (begin (proc (stream-car s))
            (stream-for-each proc (stream-cdr s)))))

;使用call-with-continuration 提前注册退出点, 在循环中判断条件, 然后退出
(define (display-stream s)
    (let ((n 0))
        (call/cc (lambda (exit)     ;exit相当于call/cc外部的环境, 调用exit相当于退出整个call/cc语句继续往下走
            (stream-for-each 
                (lambda (x) 
                    (set! n (+ n 1)) 
                    (if (< n 11) 
                        (begin (display x) (display " "))
                        (begin (display "...") (exit 'done)))) s)))
        (newline)))

(define (stream-enumerate-interval low high)
    (if (> low high) the-empty-stream
        (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream
            (apply proc (map stream-car argstreams))
            (apply stream-map
                (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
    (cons-stream
        (+ (stream-car s1) (stream-car s2))
        (add-streams (stream-cdr s1) (stream-cdr s2))))

(define (mul-stream a-stream b-stream)
    (if (stream-null? a-stream)
        the-empty-stream)
        (cons-stream
            (* (stream-car a-stream) (stream-car b-stream))
            (mul-stream (stream-cdr a-stream) (stream-cdr b-stream))))

(define (merge s1 s2)
    (cond 
        ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
            (let ((c1 (stream-car s1)) (c2 (stream-car s2)))
                (cond 
                    ((< c1 c2) (cons-stream c1 (merge (stream-cdr s1) s2)))
                    ((> c1 c2) (cons-stream c2 (merge s1 (stream-cdr s2))))
                    (else (cons-stream c1 (merge (stream-cdr s1) (stream-cdr s2)))))))))

(define (scale-stream s factor)
    (stream-map (lambda (x) (* x factor)) s))

(define (partial-sums s)
    (cons-stream 
        (stream-car s)
        (add-streams 
            (stream-cdr s)
            (partial-sums s))))


;3.81
;(define (random-numbers)
;    (cons-stream
;        random-init
;        (stream-map 
;            (lambda (x) (random-update x))
;         random-numbers)))


;(define (rand-numbers stream)
;    (lambda (m)
;        (cond ((eq? m 'generate)
;                ;取stream下一个?
;                )
;            ((eq? m 'reset)
;                (lambda (val)
;                    (
;                        ;找到流里的第一个val, 抛弃前面的值
;                    )))
;            (else (error 'rand-numbers "wrong command!"))
;        )))


;(define s1 (rand-numbers random-number-stream-1))
;(s1 'generate)
;(define s2 ((s1 'reset) 123))
;(s2 'generate)




(define (rand-generator commands)
    (define (rand-helper num remaining-commands)
        (let ((next-command (stream-car remaining-commands))) 
            (cond ((eq? next-command 'generate) 
                        (cons-stream num 
                            (rand-helper (rand-update num)
                                (stream-cdr remaining-commands)))) 
                ((pair? next-command)
                    (if (eq? (car next-command) 'reset) 
                        (cons-stream (cdr (stream-car remaining-commands)) 
                            (rand-helper (rand-update (cdr (stream-car remaining-commands))) 
                               (stream-cdr remaining-commands))) 
                        (error "bad command -- " next-commmand)))
                (else (error "bad command -- " next-commmand))))) 
    (rand-helper rand-init commands)) 

;;; testing 

;;; generate stream of commands 
(define gen-stream
    (cons-stream
        (cons 'reset 12) 
        (cons-stream 'generate 
            (cons-stream (cons 'reset 100)
                (cons-stream 'generate 
                    gen-stream)))))

(define rands (rand-generator gen-stream))

(stream-ref rands 0) 
(stream-ref rands 1) 
(stream-ref rands 2) 
(stream-ref rands 3) 
(stream-ref rands 4) 
(stream-ref rands 5) 

;output: 

;(stream-ref rands 0) 
;Value: 12 

;(stream-ref rands 1) 
;Value: 1033878523 

;(stream-ref rands 2) 
;Value: 100 

;(stream-ref rands 3) 
;Value: 1180356723 

;(stream-ref rands 4) 
;Value: 12 

; (stream-ref rands 5) 
;Value: 1033878523 



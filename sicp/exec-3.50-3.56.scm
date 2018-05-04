(library-directories "..")
(import (modules))


;stream
(define the-empty-stream `(()))

(define (stream-null? s) (equal? s the-empty-stream))

(define (stream-ref s n)
	(if (= n 0) (stream-car s)
		(stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
	(if (stream-null? s) the-empty-stream
		(cons-stream (proc (stream-car s))
			(stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
	(if (stream-null? s) `done
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
    			(let ((already-run? #f) (result #f))	;在匿名函数作用域内定义内部变量
	    			(lambda ()							;返回使用这些内部变量的函数(闭包)
		    			(if (not already-run?)
							(begin 
								(set! result ((lambda () b)))
								(set! already-run? #t)
								result)
							result))))))
    	)))

;(define (cons-stream a b) (cons a (delay b)))

(define (stream-for-each proc s)
	(if (stream-null? s) `done
		(begin (proc (stream-car s))
			(stream-for-each proc (stream-cdr s)))))

;使用call-with-continuration 提前注册退出点, 在循环中判断条件, 然后退出
(define (display-stream s)
	(let ((n 0))
		(call/cc (lambda (exit)		;exit相当于call/cc外部的环境, 调用exit相当于退出整个call/cc语句继续往下走
			(stream-for-each 
				(lambda (x) 
					(set! n (+ n 1)) 
					(if (< n 11) 
						(begin (display x) (display " "))
						(begin (display "...") (exit `done)))) s)))
		(newline)))

(define (stream-enumerate-interval low high)
	(if (> low high) the-empty-stream
		(cons-stream low (stream-enumerate-interval (+ low 1) high))))

(define (integers-starting-from n)
	(cons-stream n (integers-starting-from (+ n 1))))

(define s (cons-stream `1 `2))
(display (stream-car s))(newline)
(display (stream-cdr s))(newline)

(define i (stream-enumerate-interval 1 100))
(display-stream i)


(define integers (integers-starting-from 1))

;3.50
(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream
            (apply proc (map stream-car argstreams))
            (apply stream-map
                (cons proc (map stream-cdr argstreams))))))

;3.53
;1 2 4 8 16 ....
(define (add-streams s1 s2)
	(cons-stream
		(+ (stream-car s1) (stream-car s2))
		(add-streams (stream-cdr s1) (stream-cdr s2))))
(define s (cons-stream 1 (add-streams s s)))
(display-stream s)

;3.54
(define (mul-stream a-stream b-stream)
	(if (stream-null? a-stream)
		the-empty-stream)
		(cons-stream
			(* (stream-car a-stream) (stream-car b-stream))
			(mul-stream (stream-cdr a-stream) (stream-cdr b-stream))))

(define factorials 
	(cons-stream 
		1 
		(mul-stream
			(stream-cdr integers) ;从2开始的自然数
			factorials)))

(display-stream factorials)

;3.55
(define (partial-sums s)
	(cons-stream 
		(stream-car s)
		(add-streams 
			(stream-cdr s)
			(partial-sums s))))

(display-stream (partial-sums integers))





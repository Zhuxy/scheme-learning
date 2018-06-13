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
	(if (stream-null? s) 'done
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

(define (sqrt-improve guess x)
	((lambda (a b) (/ (+ a b) 2)) (/ x guess) guess))

(define (sqrt-stream x)
	(define guesses
		(cons-stream
			1.0
			(stream-map
				(lambda (guess) (sqrt-improve guess x))
				guesses)))
	guesses)

(display "(sqrt-stream 2): ")
(display-stream (sqrt-stream 2))


(define (pi-summands n)
	(cons-stream
		(/ 1.0 n)
		(stream-map
			-
			(pi-summands (+ n 2)))))

(define pi-stream
	(scale-stream (partial-sums (pi-summands 1)) 4))

(display-stream pi-stream)

(define (euler-transform s)
	(let ((s0 (stream-ref s 0))
			(s1 (stream-ref s 1))
			(s2 (stream-ref s 2)))
		(cons-stream
			(- s2 (/ ((lambda (x) (* x x)) (- s2 s1))
					(+ s0 (* -2 s1) s2)))
			(euler-transform (stream-cdr s)))))

(display-stream (euler-transform pi-stream))

(define (make-tableau transform s)
	(cons-stream
		s
		(make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
	(stream-map
		stream-car
		(make-tableau transform s)))

(display-stream (accelerated-sequence euler-transform pi-stream))

;3.63
;重复调用(sqrt-steam x)导致多余开销, 原方案直接利用已定义好的sqrt-stream的已生成部分

;3.64
(define (abs x) (if (< x 0) (* -1 x) x))
(define (stream-limit s t)
	(let ((s0 (stream-ref s 0))
			(s1 (stream-ref s 1)))
		(if (< (abs (- s1 s0)) t) s1
			(stream-limit (stream-cdr s) t))))

(display (stream-limit (sqrt-stream 2) 0.00000001))

;3.65
(define (ln-summands n)
	(cons-stream 
		(/ 1.0 n)
		(stream-map
			-
			(ln-summands (+ n 1)))))

(define (ln-stream n)
	(partial-sums (ln-summands 1)))

(display "(ln-stream 2)")
(display-stream (ln-stream 2))
(display (stream-limit (accelerated-sequence euler-transform (ln-stream 2)) 0.0000000001))












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
					(if (< n 16) 
						(begin (display x) (display " "))
						(begin (display "...") (exit 'done)))) s)))
		(newline)))

(define (stream-enumerate-interval low high)
	(if (> low high) the-empty-stream
		(cons-stream low (stream-enumerate-interval (+ low 1) high))))

(define (integers-starting-from n)
	(cons-stream n (integers-starting-from (+ n 1))))

(define s (cons-stream '1 '2))
(display (stream-car s))(newline)
(display (stream-cdr s))(newline)

(define i (stream-enumerate-interval 1 100))
(display "(stream-enumerate-interval 1 100): ")
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
(display "(cons-stream 1 (add-streams s s)): ")
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

(display "factorials: ")
(display-stream factorials)

;3.55
(define (partial-sums s)
	(cons-stream 
		(stream-car s)
		(add-streams 
			(stream-cdr s)
			(partial-sums s))))

(display "(partial-sums integers): ")
(display-stream (partial-sums integers))

;3.56
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

(define m (merge integers factorials))
(display "(merge integers factorials): ")
(display-stream m)


(define (scale-stream s factor)
	(stream-map (lambda (x) (* x factor)) s))

(display "(scale-stream integers 3): ")
(display-stream (scale-stream integers 3))

(define s (cons-stream 1 (merge (scale-stream integers 2) (merge (scale-stream integers 3) (scale-stream integers 5)))))
(display "stream with 2 3 5 scales: ")
(display-stream s)



;3.57
;1 1 2 3 5 8 13
(define fibs 
	(cons-stream 1
		(cons-stream 1
			(add-streams fibs
				(stream-cdr fibs)))))

(display "fibs with add-streams: ")
(display-stream fibs)

;3.58
;(expand 1 7 10)
;(1 (expand 3 7 10))
;(1 (4 (expand 2 7 10)))
;(1 (4 (2 (expand 6 7 10))))
;(1 (4 (2 (8 (expand 4 7 10)))))
;(1 (4 (2 (8 (5 (expand 5 7 10))))))
(define (expand num den radix)
	(cons-stream
		(quotient (* num radix) den)
		(expand (remainder (* num radix) den) den radix)))
(display "(expand 1 7 10): ")
(display-stream (expand 1 7 10))
(display "(expand 3 8 10): ")
(display-stream (expand 3 8 10))

;3.59 a)
(define (mul-inv-stream s)
	(cons-stream
		(/ 1 (stream-car s))
		(mul-inv-stream (stream-cdr s))))

(display "mul-inv-stream integers: ")
(display-stream (mul-inv-stream integers))

(define (integrate-series a)
	(mul-stream
		a
		(mul-inv-stream integers)))

;3.59 b)

;3.60
;3.61
;3.62












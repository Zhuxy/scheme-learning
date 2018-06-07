(library-directories "..")
(import (modules))

;4.21
(displayn "fact: "
	((lambda (n) (							;此处类似闭包, 只是把实参10通过形参n引入到环境
		(lambda (fact) (fact fact n))		;此处通过把高阶函数绑定到形参, 并将自己作为参数传给自己, 达到函数可以递归调用的目的?
		(lambda (ft k)
			(if (= k 1)
				1
				(* k (ft ft (- k 1))))))
	) 10)
)

;4.21 (a) fib函数实现
;(define (fib n)
;	(cond ((= n 0) 1) ((= n 1) 1)
;		(else (+ (fib (- n 1)) (fib (- n 2)))))
;)
;(display (fib 10))

(displayn "fib: "
	((lambda (n) (	
		(lambda (fact) (fact fact n))
		(lambda (fib k)
			(cond ((= k 0) 1) ((= k 1) 1)
				(else (+ (fib fib (- k 1)) (fib fib (- k 2)))))))
	) 10)
)
(define y
	(lambda (func)
		(lambda (x)
			((func (y func)) x))))

(define fact
	(y
		(lambda (self)
			(lambda (n)
				(if (= n 1) 1 (* n (self (- n 1))))))
	))

(displayn "fact with Y-combinator: " (fact 10))


;4.21(b)
(define (f x)
	(define (even? n)
		(if (= n 0) #t (odd? (- n 1))))
	(define (odd? n)
		(if (= n 0) #f (even? (- n 1))))
	(display (even? x))(newline))

(f 10)

(define (h x)
	((lambda (even? odd?) (even? even? odd? x))
		(lambda (ev? od? n)
			(if (= n 0) #t (od? ev? od? (- n 1))))
		(lambda (ev? od? n)
			(if (= n 0) #f (ev? ev? od? (- n 1))))
	))

(displayn "ev? od? : " (h 11))








(library-directories "..")
(import (modules))

;4.21
(displayn "fact: "
	((lambda (n) (							;此处类似闭包, 只是把实参10通过形参n引入到环境
		(lambda (fact) (fact fact n))		;此处通过把高阶函数绑定到形参, 并将自己作为参数传给自己, 达到函数可以递归调用的目的?
		(lambda (ft k)						;需要递归调用自己, 就要把自己变成参数传进来
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
	(lambda (func)				;func是一个返回函数的函数, 他的唯一参数是返回函数的函数体中需要表达递归调用自己的函数
		(lambda (x)				;这个函数是最终需要执行的逻辑, 也就是func要返回的内容
			((func (y func)) 	;(y func)就是"自己", 因为最终也是用y算子去调用实际的逻辑, 返回的函数, 如 fact = y(实际逻辑)
				x))))

(define fact
	(y							
		(lambda (self)			;对于需要递归调用自己的函数, 必须外层嵌套一个函数, 通过引入自身作为参的方式完成递归调用
			(lambda (n)
				(if (= n 1) 1 (* n (self (- n 1))))))
	))

(displayn "fact with Y-combinator: " (fact 10))

;y也需要调用自己所以在外层再包一层, y算子变成需要转换前的实际函数
(define real-y
(
	(lambda (y)
		(lambda (f)		
			(lambda (x)	
				((f ((y y) f))		;因为参数y已经变成被包装real函数的函数, 所以再用(y y)获取实际的函数, 及y算子
					x)))
	)
	(lambda (y)
		(lambda (f)		
			(lambda (x)	
				((f ((y y) f))
					x)))
	)
))

(define fact2
	(real-y							
		(lambda (self)
			(lambda (n)
				(if (= n 1) 1 (* n (self (- n 1))))))
	))
(displayn "fact2 with real Y-combinator: " (fact2 10))


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


(define Y
	(lambda (f)
		((lambda (x)
			((f x) x))
		(lambda (x)
			((f x) x)))
	)
)
(define fact3
	(Y						
		(lambda (self)
			(lambda (n)
				(if (= n 1) 1 (* n (self (- n 1))))))
	))
(displayn "fact3 with Y: " (fact3 10))

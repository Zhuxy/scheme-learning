(library-directories "..")
(import (modules))

;5.1
(define (factorial n)
    (define (iter product counter)
        (if (> counter n)
            product
            (iter (* counter product)
                (+ counter 1))))
    (iter 1 1))

(displayn "factorial 6: " (factorial 6))

;5.2
(controller
    ; (assign n (const 6))
    (assign product (const 1))
    (assign counter (const 1))
    factorial-start
        (test (op > (reg counter) (reg n))) ; 设置标志位为1
        (branch (label factorial-done)) ;标志位为1时执行
        (assign product (op *) (reg counter) (reg product))
        (assign counter (op +) (reg counter) (const 1))
        (goto (label factorial-start))
    
    factorial-done
)

;5.3
(define (sqrt x)
    (define (good-enough? guess)
        (< (abs (- (square guess) x)) 0.001))
    (define (improve guess)
        (average guess (/ x guess)))
    (define (sqrt-iter guess)
        (if (good-enough? guess)
            guess
            (sqrt-iter (improve guess))))
    (sqrt-iter 1.0)
)

(controller
    ; (assign x 15)
    (assign g (const 1.0))
    sqrt-loop
        (assign t (op *) (reg g) (reg g))
        (assign t (op -) (reg t) (reg x))
        (test (op <) (reg t) (const 0))
        (branch
            (assign t (op *) (reg t) (const -1))
        )
        (test (op <) (reg t) (const 0.001))
        (branch (label sqrt-done))
        (assign t (op /) (reg x) (reg g))
        (assign g (op average) (reg g) (reg t))
        (goto (label sqrt-loop))
    
    sqrt-done
)

;递归计算factorial
(define (factorial n)
    (if (= n 1)
        1
        (* n (factorial (- n 1)))))

;使用堆栈解决子程序嵌套调用问题
(controller
    (assign continue (label fact-done))
    fact-loop
        (test (op =) (reg n) (const 1))
        (branch (label fact-base))
        (save continue)
        (save n)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-fact))
        (goto (label fact-loop))
    after-fact
        (restore n)
        (restore continue)
        (assign val (op *) (reg n) (reg val))
        (goto (reg continue))
    fact-base
        (assign val (const 1))
        (goto (label continue))
    fact-done
)

;双重递归计算斐波那契数
(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1) (- n 2)))))

(controller
    (assign continue (label fib-done))
    fib-loop
        (test (op <) (reg n) (const 2))
        (branch (label fib-base))
        (save continue)
        (save n)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-fib-1))
        (goto (label fib-loop))
    after-fib-1
        (restore n)
        (restore continue)
        (assign n (op -) (reg n) (const 2))
        (save continue)
        (assign continue (lable after-fib-2))
        (save val)
        (goto (label fib-loop))
    after-fib-2
        (assign n (reg val))
        (restore val)
        (assign val (op +) (reg val) (reg n))
        (goto (reg continue))
    fib-base
        (assign val (reg n))
        (goto (reg continue))    
    fib-done
)
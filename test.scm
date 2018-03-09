;使用闭包的方式实现negerator
(define (generator start end)
    (let 
        ((curr (- start 1)))
        (lambda ()
            (set! curr (+ curr 1))
            (if (> curr end) #f curr))))


(define next (generator 0 3))
(display (next))(newline)
(display (next))(newline)
(display (next))(newline)
(display (next))(newline)
(display (next))(newline)
(display (next))(newline)


;accm list init fn collector
(define acc
    (lambda (list init fn collector)
        (cond 
            ((null? list) (collector init))
            (else 
                (acc
                    (cdr list)
                    (+ init (fn (car list)))
                    fn
                    collector)))))

(display (acc `() 0 (lambda (x) (* 2 x)) (lambda (x) x)))(newline)
(display (acc `(1 2 3) 0 (lambda (x) (* 2 x)) (lambda (x) x)))(newline)

(display "accumulator with continuation passing style??")(newline)
(define acc-cps
    (lambda (list init fn collector)
        (if 
            (null? list) (collector init);如果是空元素, 则中止当前的计算
            (fn (car list)
                ;这里就是continuation, 表示之后的所有计算过程
                ;包含了我当前的计算结果, 交给之后的迭代去计算剩余的?
                (lambda (sum)
                    (collector (+ sum
                        (acc-cps
                            (cdr list)
                            init
                            fn
                            collector))))
            ))))

(display 
    (acc-cps `(1 2 3 4) 100
        (lambda (x k) (k (* 2 x)))
        (lambda (x) x)))(newline)

(define acc-cc
    (lambda (list init fn collector)
        (if (null? list) (collector init)
            (collector (+ (fn (car list)) (call/cc (lambda (k) (k (acc-cc (cdr list) init fn collector)))) ))
        )
    ))

(display 
    (acc-cc `(1 2 3 4) 100
        (lambda (x) (* 2 x))
        (lambda (x) x)))(newline)

(define (map-k f xs k)
  (if (null? xs) (k '())
      (f (car xs) (lambda (v) (map-k f (cdr xs)
                                  (lambda (rest-v) (k (cons v rest-v))))))))

(display (map-k 
    (lambda (x k) (k (+ x 1))) 
    '(1 2 3)
    (lambda (x) x)))(newline)

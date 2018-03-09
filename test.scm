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
(trace-define acc
    (lambda (list init fn collector)
        (cond 
            ((null? list) (collector init))
            (else 
                (acc
                    (cdr list)
                    (+ init (fn (car list)))
                    fn
                    collector)))))

(display (acc `(1 2 3) 0 (lambda (x) (* 2 x)) (lambda (x) x)))

;acc with continuration??



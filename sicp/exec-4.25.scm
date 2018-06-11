;4.25
;应用序会提前执行递归部分，如果n为1则会计算fact 0导致死循环

;4.26
;(unless condition usual-value exceptional-value)
;=>
;(let ((usual (lambda () usual-value))
;      (exception (lambda () exceptional-value)))
;  (if condition (usual) (exception)))

(define-syntax unless
  (syntax-rules ()
    ((unless conditon usual-value exception-value)
     	(let ((usual (lambda () usual-value))
	      (exception (lambda () exceptional-value)))
	  (if condition (usual) (exception))))))

(display (unless (> 5 1) (begin (display `a) `b) (begin (display `c) `d)))

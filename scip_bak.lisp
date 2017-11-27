;;;;;;; 2.6

(define zero
	(lambda (f) (lambda (x) x)))

(define add-1
	(lambda (n) 
		(lambda (f) 
			(lambda (x) (f ((n f) x))))))

(define one (add-1 zero))

(define one
	(lambda (f) 
			(lambda (x) (f x))))

(define two (add-1 one))

(define two
	(lambda (f) 
			(lambda (x) (f (f x)))))

(define add
	(lambda (m n) 
		(lambda (f) 
			(lambda (x) ((m f) ((n f) x))))))

(define inc
	(lambda (n) (+ n 1)))

(display ((zero inc) 0))
(newline)
(display ((one inc) 0))
(newline)
(display ((two inc) 0))
(newline)
(display (((add two one) inc) 0))

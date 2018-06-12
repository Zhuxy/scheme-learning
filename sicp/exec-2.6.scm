;;;;;;; exec 2.6 丘奇计数法

(define zero
	(lambda (f x) x))

(define add-1
	(lambda (n) 
		(lambda (f x) 
			(f ((n f) x)))))

(define one (add-1 zero))

(define one
	(lambda (f x) 
			(f x)))

(define two (add-1 one))

(define two
	(lambda (f x) 
			(f (f x))))

(define add
	(lambda (m n) 
		(lambda (f x) 
			(m f (n f x)))))

(define inc
	(lambda (n) (+ n 1)))

(display (zero inc 0))
(newline)
(display (one inc 0))
(newline)
(display (two inc 0))
(newline)
(display ((add two one) inc 0))

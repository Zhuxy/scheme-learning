(library (modules)
	(export displayn square)

	(import (rnrs))

	(define displayn
	    (lambda (q s)
	        (begin
	            (display q)
	            (display s)
	            (newline))))

	(define (square x) (* x x))
)
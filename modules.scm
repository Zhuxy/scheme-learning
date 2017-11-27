(library (modules)
	(export displayn)

	(import (rnrs))

	(define displayn
	    (lambda (q s)
	        (begin
	            (display q)
	            (display s)
	            (newline))))
)
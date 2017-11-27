(let
	(
		(f 
			(lambda (x) 
				(cond 
					((= 1 x) (display x))
					(else (display 0))
				)
			)
		)
	)
	(f 1)
)


(letrec 
    (
        (fringe-it
            (lambda (source target)
                (cond 
                    ((null? source) target)
                    ((atom? source) (cons source target))
                    (else 
                        (append 
                        	(fringe-it (car source) `()) 
                        	(fringe-it (cdr source) `())
                        )
                    )
                )
            )
        )
    )
    (fringe-it `(1 2 3) `())
)
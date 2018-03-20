(library-directories "..")
(import (modules))

(define (element-of-set? x set)
	(cond ((null? set) #f)
		((equal? x (car set)) #t)
		(else (element-of-set? x (cdr set)))))

(displayn "element-of-set?: " (element-of-set? `1 `(1 2 3)))
(displayn "element-of-set?: " (element-of-set? `4 `(1 2 3)))

(define (adjoin-set x set)
  	(if (element-of-set? x set) set
  		(cons x set)))

(define (intersection-set set1 set2)
	(cond ((or (null? set1) (null? set2)) `())
	      ((element-of-set? (car set1) set2) 
	      		(cons (car set1) (intersection-set (cdr set1) set2)))
	  	  (else (intersection-set (cdr set1) set2))))

(displayn "intersection-set: " (intersection-set `(1 2) `(3 4)))
(displayn "intersection-set: " (intersection-set `(1 2 3) `(2 3 4)))

;2.59
(define (union-set set1 set2)
	(cond 
		((null? set1) set2)
		((null? set2) set1)
		((not (element-of-set? (car set1) set2))
			(cons (car set1) (union-set (cdr set1) set2)))
		(else (union-set (cdr set1) set2))))

(displayn "union-set: " (union-set `(1 2) `(3 4)))
(displayn "union-set: " (union-set `(1 2 3) `(2 3 4)))
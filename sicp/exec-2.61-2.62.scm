(library-directories "..")
(import (modules))

(define (element-of-set? x set)
	(cond ((null? set) #f)
		((= x (car set)) #t)
		((< x (car set)) #f)
		(else (element-of-set? x (cdr set)))))

(displayn "element-of-set?: " (element-of-set? `1 `(1 2 3)))
(displayn "element-of-set?: " (element-of-set? `4 `(1 2 3)))

(define (intersection-set set1 set2)
	(if (or (null? set1) (null? set2)) `()
		(let ((x1 (car set1)) (x2 (car set2)))
			(cond ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
				((< x1 x2) (intersection-set (cdr set1) set2))
				((> x1 x2) (intersection-set set1 (cdr set2)))))))

(displayn "intersection-set: " (intersection-set `(1 2) `(3 4)))
(displayn "intersection-set: " (intersection-set `(1 2 3 4 5 6) `(7 8 9 10 11 12 13)))

;2.61
(define (adjoin-set x set)
	(cond ((null? set) (cons x `()))
		((= x (car set)) set)
		((< x (car set)) (cons x set))
		((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))))

(displayn "adjoin-set: " (adjoin-set `3 `(1 2 4 5)))

;2.62
(define (union-set set1 set2)
	(cond 
		((null? set1) set2)
		((null? set2) set1)
		(else (let ((x1 (car set1)) (x2 (car set2)))
			(cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
				((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
				((> x1 x2) (cons x2 (union-set set1 (cdr set2)))))))))

(displayn "union-set: " (union-set `(1 2) `(3 4)))
(displayn "union-set: " (union-set `(1 2 3 5 6) `(2 3 4 7 8 9)))
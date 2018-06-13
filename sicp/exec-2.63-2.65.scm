(library-directories "..")
(import (modules))

;tree
(define (entry tree)
	(car tree))

(define (left-branch tree)
	(cadr tree))

(define (right-branch tree)
	(caddr tree))

(define (make-tree entry left-branch right-branch)
	(list entry left-branch right-branch))

(define  (tree->list-1 tree)
	(if (null? tree) '()
		(append (tree->list-1 (left-branch tree))
			(cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
	(define (copy-to-list tree result-list)
		(if (null? tree) result-list
			(copy-to-list (left-branch tree)
				(cons (entry tree) 
					(copy-to-list (right-branch tree) result-list)))))
	(copy-to-list tree ' ()))

(define tree '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))

(displayn "tree->list-1: " (tree->list-1 tree))
(displayn "tree->list-2: " (tree->list-1 tree))


;2.64
(define (list->tree elements)
	(car (partial-tree elements (length elements))))

(define (partial-tree elts n)
	(if (= n 0) (cons '() elts)
		(let ((left-size (quotient (- n 1) 2)))
			(let ((left-result (partial-tree elts left-size)))
				(let ((left-tree (car left-result))
						(non-left-elts (cdr left-result))
						(right-size (- n (+ left-size 1))))
					(let ((this-entry (car non-left-elts))
							(right-result (partial-tree (cdr non-left-elts) right-size)))
						(let ((right-tree (car right-result))
								(remaining-elts (cdr right-result)))
							(cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))

(displayn "partial-tree: " (partial-tree '(4 5 6 7 8 9 10 11 12 13) 7))

;2.65
(define (union-list l1 l2) 
	(cond ((null? l1) l2)
		((null? l2) l1) 
		(else
			(let ((e1 (car l1)) (e2 (car l2)))
				(cond 
					((= e1 e2) (cons e1 (union-list (cdr l1) (cdr l2))))
					((> e1 e2) (cons e2 (union-list l1 (cdr l2))))
					((< e1 e2) (cons e1 (union-list (cdr l1) l2))))))))

(define (union-set set1 set2)
	(let ((elts1 (tree->list-1 set1)) (elts2 (tree->list-1 set2)))
			(let ((all-list (union-list elts1 elts2)))
				(list->tree all-list))))

(displayn "union-set: " (union-set 
	'(3 (1 () (2 () ())) (5 (4 () ()) (6 () ())))
	'(10 (8 (7 () ()) (9 () ())) (12 (11 () ()) (13 () ())))))

(define (intersection-list set1 set2)
	(if (or (null? set1) (null? set2)) '()
		(let ((x1 (car set1)) (x2 (car set2)))
			(cond ((= x1 x2) (cons x1 (intersection-list (cdr set1) (cdr set2))))
				((< x1 x2) (intersection-list (cdr set1) set2))
				((> x1 x2) (intersection-list set1 (cdr set2)))))))

(define (intersection-set set1 set2)
	(let ((elts1 (tree->list-1 set1)) (elts2 (tree->list-1 set2)))
			(let ((all-list (intersection-list elts1 elts2)))
				(list->tree all-list))))

(displayn "intersection-set: " (intersection-set 
	'(3 (1 () (2 () ())) (5 (4 () ()) (6 () ())))
	'(7 (5 (4 () ()) (6 () ())) (9 (8 () ()) (10 () ())))))


(trace-define (lookup given-key tree-of-records)
	(cond ((null? tree-of-records) #f)
		((eq? given-key (entry tree-of-records)) #t)
		(else (or (lookup given-key (left-branch tree-of-records)) (lookup given-key (right-branch tree-of-records))))))

(displayn "lookup in tree: " (lookup 9 tree))


(library-directories "..")
(import (modules))

(define (list-ref list i) 
	(if (= i 0) (car list)
		(list-ref (cdr list) (- i 1))))

(displayn "list-ref: " (list-ref '(a b c) 1))

(define list-length
    (lambda (list)
        (cond 
            ((null? list) 0)
            (else 
                (+ 1 (list-length (cdr list)))))))
(displayn "list-length: " (list-length '(a b c d)))


(define (append list1 list2)
	(if (null? list1) list2
		(cons (car list1) (append (cdr list1) list2))))

(displayn "append: " (append '(1 2 3) '(a b c)))

(define (last-pair list) 
	(if (null? (cdr list)) (car list)
		(last-pair (cdr list))))

(displayn "last-pair: " (last-pair '(1 2 3 4)))

(define reverse
	(lambda (list)
            (define reverse-it 
                (lambda (source target)
                    (if (null? source) target
                        (reverse-it (cdr source) (cons (car source) target)))))
            (reverse-it list '())))

(displayn "reverse: " (reverse '(a b c d e)))

(define list-filter
    (lambda (list predic) 
        (cond 
            ((null? list) '())
            ((predic (car list)) (cons (car list) (list-filter (cdr list) predic)))
            (else (list-filter (cdr list) predic)))))

;2.20
(define same-parity
    (lambda (a . list)
        (cons a (list-filter list (lambda (i) (even? (- i a)))))))

(displayn "same-parity: " (same-parity 1 2 3 4 5 6 7 8 9))

(define for-each
    (lambda (list consumer) 
        (if (not (null? list))
            (begin (consumer (car list)) (for-each (cdr list) consumer)))))

(displayn "for-each: " (for-each '(1 2 3 4 5) (lambda (x) (display x))))

;2.27
(define deep-reverse
    (lambda (list)
            (define reverse-it 
                (lambda (source target)
                    (cond
                        ((null? source) target)
                        ((atom? source) source)
                        (else (reverse-it (cdr source) (cons (reverse-it (car source) '()) target))))))
            (reverse-it list '())))

(displayn "deep-reverse: " (deep-reverse '(2 (3 4 5) ((6 7 (8 9)) 10))))


;2.28
(define fringe
    (lambda (list)
        (letrec 
            ((fringe-it
                    (lambda (source target)
                        (cond 
                            ((null? source) target)
                            ((atom? source) (cons source target))
                            (else (append (fringe-it (car source) '()) (fringe-it (cdr source) '())))))))
            (fringe-it list '()))))

(displayn "fringe: " (fringe '(1 2 (3 4 (5 6 (7 8))))))

;2.32
(define subsets
    (lambda (set)
        (if (null? set) (list '())
            (let ((rest (subsets (cdr set))))
                (append rest
                    (map (lambda (x) (append (list (car set)) x)) rest))))))

(displayn "subsets: " (subsets '(1 2 3)))

;2.33
(define accumulate
    (lambda (op initial sequence)
        (cond
            ((null? sequence) initial)
            (else (op (car sequence) (accumulate op initial (cdr sequence)))))))

(displayn "accumulate: " (accumulate cons '(0) '(1 2 3 4)))

(define (map-acc p sequence)
    (accumulate (lambda (x y) (append (list (p x)) y)) '() sequence))

(displayn "map-acc: " (map-acc (lambda (x) (- x 1)) '(1 2 3 4)))

(define (append-acc seq1 seq2)
    (accumulate cons seq2 seq1))

(displayn "append-acc: " (append-acc '(1 2 3) '(a b c)))

(define (length-acc sequence)
    (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(displayn "length-acc: " (length-acc '(1 2 3 4)))

;2.34
(define (honer-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms) this-coeff)) 0 coefficient-sequence))

(displayn "honer-eval: " (honer-eval 2 '(1 3 0 5 0 1)))


;2.35
(define (count-leaves tree)
    (cond 
        ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                  (count-leaves (cdr tree))))))

(displayn "count-leaves: " (count-leaves '(1 (2 3) (4 (5 (6 7))) 8 (9 10))))

(define (count-leaves-acc tree)
    (accumulate 
            (lambda (x y) (+ x y))
            0 
            (map 
                (lambda (n)
                    (cond 
                        ((null? n) 0)
                        ((atom? n) 1)
                        (else (+ (count-leaves-acc (list (car n))) (count-leaves-acc (cdr n))))))
                tree)))

(displayn "count-leaves-acc: " (count-leaves-acc '(1 (2 3) (4 (5 (6 7))) 8 (9 10))))

;2.36
(define (accumulate-n op initial seqs)
    (if (null? (car seqs)) '()
        (cons (accumulate op initial (map car seqs))
                (accumulate-n op initial (map cdr seqs)))))

(displayn "accumulate-n: " (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))


;2.37
(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(displayn "dot-product: " (dot-product '(1 2 3) '(4 5 6)))

(define (matrix-*-vector m v)
    (map (lambda (x) (dot-product x v)) m))

(displayn "matrix-*-vector: " 
        (matrix-*-vector
            '((1 2 3) (4 5 6))
            '(1 2 3)))

(define (transpose mat)
    (accumulate-n cons '() mat))

(displayn "transpose: " (transpose '((1 2 3) (4 5 6))))


(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (x) (matrix-*-vector m x)) m)))

(displayn "matrix-*-matrix: " 
        (matrix-*-matrix
            '((1 2 3) (4 5 6))
            '((1 4) (2 5) (3 6))))

;2.38
(define (fold-right op initial sequence)
    (cond 
        ((null? sequence) initial)
        (else (op (car sequence) (fold-right op initial (cdr sequence))))))

(define (fold-left op initial sequence)
    (define (iter result rest)
       (if (null? rest) result
            (iter (op result (car rest)) (cdr rest))))
    (iter initial sequence))

(displayn "fold-right: " (fold-right / 1 '(1 2 3)))
(displayn "fold-left: " (fold-left / 1 '(1 2 3)))

;2.39
(define temp
    (lambda (x y)
        (append y (list x))))

(define (reverse-fold-right list)
    (fold-right 
            ;(lambda (x y) (append y (list x)))
            temp
            '() list))

(displayn "reverse-fold-right: " (reverse-fold-right '(1 2 3 4 5)))

(define (reverse-fold-left list)
    (fold-left 
            (lambda (x y) (cons y x)) 
            '() list))

(displayn "reverse-fold-left: " (reverse-fold-left '(1 2 3 4 5)))


(define (flatmap proc sequence)
    (accumulate append '()
        (map proc sequence)))

(displayn "flatmap: " 
    (flatmap
        (lambda (x) (map (lambda (y) (* y 2)) x)) '((1 2 3) (4 5 6) (7 8 9))))

(define (remove x s)
    (filter (lambda (y) (not (= x y))) s))

(displayn "remove: " (remove 1 '(2 1 3 4)))

(define (permutations s)
    (if (null? s) '(())
        (flatmap 
                (lambda (x) 
                    (map (lambda (p) (cons x p))
                        (permutations (remove x s))))
            s)))

(displayn "permutations: " (permutations '(1 2 3)))

;2.40
(define (enumerate-interval l h)
    (if (> l h) '()
        (cons l (enumerate-interval (+ l 1) h))))

(displayn "enumerate-interval: " (enumerate-interval 1 10))

(define (unique-pairs n)
    (flatmap 
        (lambda (x)
            (map (lambda (p) (list x p))
                (enumerate-interval (+ x 1) n)))
        (enumerate-interval 1 n)))

(displayn "unique-pairs: " (unique-pairs 5))

;2.41
(define (make-triples n)
    (flatmap
        (lambda (x)
            (map (lambda (p) (append x (list p)))
                (enumerate-interval (+ (cadr x) 1) n)))
        (unique-pairs n)))

(displayn "make-triples: " (make-triples 5))

(define (menq e list)
    (cond 
        ((null? list) #f)
        ((eq? (car list) e) list)
        (else (memq e (cdr list)))))

(displayn "memq: " (memq 'b '(a (b c) d b e)))

;2.54
(define (equal? list1 list2)
    (cond 
        ((eq? list1 list2) #t)
        ((and (list? list1) (list? list2) (eq? (car list1) (car list2))) 
                (equal? (cdr list1) (cdr list2)))
        (else #f)))

(displayn "equal?: " (equal? '(a b c) '(a b c)))
(displayn "equal?: " (equal? 'a 'a))

(displayn "equal?: " (equal? (quote (quasiquote abracadabra)) ''abracadabra))





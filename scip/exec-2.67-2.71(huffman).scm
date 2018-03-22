(library-directories "..")
(import (modules))

;huffman tree

;叶子结构(`leaf 符号 权重)
(define (make-leaf symbol weight)
 	(list `leaf symbol weight))

(define (leaf? node)
	(eq? (car node) `leaf))

(define (symbol-leaf leaf)
 	(cadr leaf))

(define (weight-leaf leaf)
 	(caddr leaf))

;huffman树结构(左 右 符号 权重)
(define (make-code-tree left right)
	(list left right
		(append (symbols left) (symbols right))
		(+ (weight left) (weight right))))

(define (left-branch tree)
	(car tree))

(define (right-branch tree)
	(cadr tree))

(define (symbols tree)
	(if (leaf? tree) (list (symbol-leaf tree)) (caddr tree)))

(define (weight tree)
	(if (leaf? tree) (weight-leaf tree) (cadddr tree)))

;辅助函数 根据01选择左右分支
(define (choose-branch bit tree)
	(cond ((= bit 0) (left-branch tree))
		((= bit 1) (right-branch tree))
		(else (error "wrong bit -- choose-branch" bit))))

;解码算法, 输入010101序列和huffman树
(define (decode bits tree)
	(if (null? bits) `()
		(let ((next-branch (choose-branch (car bits) tree)))
			(if (leaf? next-branch)
				(cons (symbol-leaf next-branch) (decode (cdr bits) tree))
				(decode (cdr bits) next-branch)))))

;开始实现huaffman算法来构造huffman树
;输入是带权重的元素集合(符号 权重)
;先创建叶子集合, 等待归并
(define (make-leaf-set pairs)
	(if (null? pairs) `()
		(cons (make-leaf (caar pairs) (cadar pairs)) (make-leaf-set (cdr pairs)))))

(displayn "make-leaf-set: " (make-leaf-set `((a 1) (b 2) (c 3))))

;归并算法过程中需要组合叶子与子树
(define (adjoin-set x set)
	(cond ((null? set) (list x))
		((< (weight x) (weight (car set))) (cons x set))
		(else (cons (car set) (adjoin-set x (cdr set))))))

(displayn "adjoin-set: " (adjoin-set `(leaf d 4) (make-leaf-set `((a 1) (b 2) (c 3) (e 5)))))


;2.67
(define sample-tree
	(make-code-tree 
		(make-leaf `a 4)
		(make-code-tree 
			(make-leaf `b 2)
			(make-code-tree 
				(make-leaf `d 1)
				(make-leaf `c 1)))))

(define sample-message `(0 1 1 0 0 1 0 1 0 1 1 1 0))

(displayn "decode: " (decode sample-message sample-tree))





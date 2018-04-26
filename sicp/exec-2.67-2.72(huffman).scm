(library-directories "..")
(import (modules))

;huffman tree

;叶子结构(`leaf 符号 权重)
(define (make-leaf symbol weight)
 	(list `leaf symbol weight))

(define (leaf? node)
	(and (not (null? node)) (eq? (car node) `leaf)))

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
	(define (decode-l bits current-branch)
		(if (null? bits) `()
			(let ((next-branch (choose-branch (car bits) current-branch)))
				(if (leaf? next-branch)
					(cons (symbol-leaf next-branch) (decode (cdr bits) tree))
					(decode-l (cdr bits) next-branch)))))
	(decode-l bits tree))

;开始实现huaffman算法来构造huffman树
;输入是带权重的元素集合(符号 权重)

;归并算法过程中需要组合叶子与子树
(define (adjoin-set x set)
	(cond ((null? set) (list x))
		((< (weight x) (weight (car set))) (cons x set))
		(else (cons (car set) (adjoin-set x (cdr set))))))

;先创建叶子集合, 等待归并
(define (make-leaf-set pairs)
	(if (null? pairs) `()
		(adjoin-set (make-leaf (caar pairs) (cadar pairs)) (make-leaf-set (cdr pairs)))))
		;(let ((pair (car pairs)))
		;	(adjoin-set (make-leaf (car pair) (cadr pair))
		;			(make-leaf-set (cdr pairs))))))

(displayn "make-leaf-set: " (make-leaf-set `((a 1) (b 2) (c 3))))



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

;2.68
;从一串符号和一颗huffman树编码成0101串
(define (encode message tree)
	(if (null? message) `()
		(append (encode-symbol (car message) tree)
			(encode (cdr message) tree))))

(define (find-in-tree given tree)
	(if (and (leaf? tree) (eq? given (symbol-leaf tree))) #t
		(not (eq? (find (lambda (x) (eq? x given)) (symbols tree)) #f))))

(displayn "find-in-tree: " (find-in-tree `a sample-tree))


(define (encode-symbol symbol tree)
	(if (and (leaf? tree) (eq? symbol (symbol-leaf tree))) `()
		(let ((left (left-branch tree)) (right (right-branch tree)))
			(cond ((find-in-tree symbol left) (cons `0 (encode-symbol symbol left)))
				((find-in-tree symbol right) (cons `1 (encode-symbol symbol right)))
				(else (error "symbol not found in tree" symbol))
			))))

(displayn "encode-symbol: " (encode-symbol `a `(leaf a 4)))

;(define (encode-symbol-l symbol tree result)
;	(if (null? tree) result
;		(let ((left (left-branch tree)) (right (right-branch tree)))
;			(cond 
;				((and (leaf? left) 
;						(eq? symbol (symbol-leaf left))) 
;						(append result `(0)))
;				((and (leaf? right) 
;						(eq? symbol (symbol-leaf right))) 
;						(append result `(1)))
;				((and (leaf? left) 
;						(not (eq? symbol (symbol-leaf left))))
;						(encode-symbol-l symbol right (append `(1) result)))
;				((and (leaf? right) 
;						(not (eq? symbol (symbol-leaf right))))
;						(encode-symbol-l symbol left (append `(0) result)))
;				(else (append result (encode-symbol-l symbol left `()) (encode-symbol-l symbol right `())))
;			))))

;(define (encode-symbol symbol tree)
;	(encode-symbol-l symbol tree `()))

;(displayn "encode-symbol-l: " (encode-symbol-l `c `((leaf d 1) (leaf c 1) (d c) 2) `()))

(displayn "encode-symbol" (encode `(a d a b b c a) sample-tree))
		
;2.69
;生成huffman树
(define (generate-huffman-tree pairs)
	(successive-merge (make-leaf-set pairs)))

;使用归并算法和make-code-tree
(define (successive-merge leafs)
	(if (null? (cddr leafs)) (make-code-tree (car leafs) (cadr leafs)) ;只有2个元素直接合成
		;合并前2个元素, 与剩余的排个序, 再递归
		(let ((head (make-code-tree (car leafs) (cadr leafs))))
			(successive-merge (adjoin-set head (cddr leafs))))))

(displayn "successive-merge" (successive-merge `((leaf a 1) (leaf b 2) (leaf c 3) (leaf d 4))))

(displayn "generate-huffman-tree" (generate-huffman-tree `((a 1) (b 2) (c 1) (d 5) (e 3) (f 4) (g 7))))

;2.70
(define rock-pairs `((a 2) (na 16) (boom 1) (sha 3) (get 2) (yip 9) (job 2) (wah 1)))
(define rock-huffman-tree (generate-huffman-tree rock-pairs))
(displayn "rock-huffman-tree: " rock-huffman-tree)
(displayn "encode get a job: " (encode `(get a job) rock-huffman-tree))
(displayn "encode sha na na na na na na na na: " (encode `(sha na na na na na na na na) rock-huffman-tree))
(displayn "encode wah yip yip yip yip yip yip yip yip yip: " (encode `(wah yip yip yip yip yip yip yip yip yip) rock-huffman-tree))
(displayn "encode sha boom: " (encode `(sha boom) rock-huffman-tree))


;2.71
;1 2 4 8 16 ... 2^(n-1)
;归并算法最频繁字符必然是0或1?
;最长编码依赖于归并的次数, n?

(displayn "generate-huffman-tree" (generate-huffman-tree `((a 1) (b 2) (c 4) (d 8) (e 16))))

;2.72
;算法复杂度是 2n
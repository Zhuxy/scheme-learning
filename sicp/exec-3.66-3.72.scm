(library-directories "..")
(import (modules))


;stream
(define the-empty-stream '(()))

(define (stream-null? s) (equal? s the-empty-stream))

(define (stream-ref s n)
    (if (= n 0) (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
    (if (stream-null? s) the-empty-stream
        (cons-stream (proc (stream-car s))
            (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
    (if (stream-null? s) 'done
        (begin (proc (stream-car s))
            (stream-for-each proc (stream-cdr s)))))

(define (memo-proc proc)
    (let ((already-run? #f) (result #f))
        (lambda ()
            (if (not already-run?)
                (begin 
                    (set! result (proc))
                    (set! already-run? #t)
                    result)
                result))))

(define (delay exp) (lambda () exp))

(define (force delayed-object) (delayed-object))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

;必须使用macro来实现, 因为直接cons a (delay d), 会优先对第二个参数b求值再调用delay
(define-syntax cons-stream
    (syntax-rules ()
        ((cons-stream a b)
            (cons a ((lambda () 
                (let ((already-run? #f) (result #f))    ;在匿名函数作用域内定义内部变量
                    (lambda ()                          ;返回使用这些内部变量的函数(闭包)
                        (if (not already-run?)
                            (begin 
                                (set! result ((lambda () b)))
                                (set! already-run? #t)
                                result)
                            result))))))
        )))

;(define (cons-stream a b) (cons a (delay b)))

(define (stream-for-each proc s)
    (if (stream-null? s) 'done
        (begin (proc (stream-car s))
            (stream-for-each proc (stream-cdr s)))))

;使用call-with-continuration 提前注册退出点, 在循环中判断条件, 然后退出
(define (display-stream s)
    (let ((n 0))
        (call/cc (lambda (exit)     ;exit相当于call/cc外部的环境, 调用exit相当于退出整个call/cc语句继续往下走
            (stream-for-each 
                (lambda (x) 
                    (set! n (+ n 1)) 
                    (if (< n 11) 
                        (begin (display x) (display " "))
                        (begin (display "...") (exit 'done)))) s)))
        (newline)))

(define (stream-enumerate-interval low high)
    (if (> low high) the-empty-stream
        (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream
            (apply proc (map stream-car argstreams))
            (apply stream-map
                (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
    (cons-stream
        (+ (stream-car s1) (stream-car s2))
        (add-streams (stream-cdr s1) (stream-cdr s2))))

(define (mul-stream a-stream b-stream)
    (if (stream-null? a-stream)
        the-empty-stream)
        (cons-stream
            (* (stream-car a-stream) (stream-car b-stream))
            (mul-stream (stream-cdr a-stream) (stream-cdr b-stream))))

(define (merge s1 s2)
    (cond 
        ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
            (let ((c1 (stream-car s1)) (c2 (stream-car s2)))
                (cond 
                    ((< c1 c2) (cons-stream c1 (merge (stream-cdr s1) s2)))
                    ((> c1 c2) (cons-stream c2 (merge s1 (stream-cdr s2))))
                    (else (cons-stream c1 (merge (stream-cdr s1) (stream-cdr s2)))))))))

(define (scale-stream s factor)
    (stream-map (lambda (x) (* x factor)) s))

(define (partial-sums s)
    (cons-stream 
        (stream-car s)
        (add-streams 
            (stream-cdr s)
            (partial-sums s))))

(define (sqrt-improve guess x)
    ((lambda (a b) (/ (+ a b) 2)) (/ x guess) guess))

(define (sqrt-stream x)
    (define guesses
        (cons-stream
            1.0
            (stream-map
                (lambda (guess) (sqrt-improve guess x))
                guesses)))
    guesses)

(display "(sqrt-stream 2): ")
(display-stream (sqrt-stream 2))


(define (pi-summands n)
    (cons-stream
        (/ 1.0 n)
        (stream-map
            -
            (pi-summands (+ n 2)))))

(define pi-stream
    (scale-stream (partial-sums (pi-summands 1)) 4))

(display-stream pi-stream)

(define (euler-transform s)
    (let ((s0 (stream-ref s 0))
            (s1 (stream-ref s 1))
            (s2 (stream-ref s 2)))
        (cons-stream
            (- s2 (/ ((lambda (x) (* x x)) (- s2 s1))
                    (+ s0 (* -2 s1) s2)))
            (euler-transform (stream-cdr s)))))

(display-stream (euler-transform pi-stream))

(define (make-tableau transform s)
    (cons-stream
        s
        (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
    (stream-map
        stream-car
        (make-tableau transform s)))

(display-stream (accelerated-sequence euler-transform pi-stream))

(define (pairs s t)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (interleave
            (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
            (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2)
    (if (stream-null? s1) s2
        (cons-stream (stream-car s1)
            (interleave s2 (stream-cdr s1)))))

(display "(pairs integers integers): ")
(display-stream (pairs integers integers))

(define (stream-filter predicate stream)
    (cond ((stream-null? stream) the-empty-stream)
        ((predicate (stream-car stream))
            (cons-stream (stream-car stream)
                (stream-filter predicate (stream-cdr stream))))
            (else (stream-filter predicate (stream-cdr stream)))))

;3.66

;3.67
(define (pairs-2 s t)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (interleave
            (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
            (interleave
                (stream-map (lambda (x) (list (stream-car t) x)) (stream-cdr s))
                (pairs-2 (stream-cdr s) (stream-cdr t))))))

(display "(pairs-2 integers integers): ")
(display-stream (pairs-2 integers integers))


;3.68
(define (pairs-3 s t)
    (interleave
            (stream-map (lambda (x) (list (stream-car s) x)) t) ; stream-car s递归调用了, 死循环1
            (pairs-3 (stream-cdr s) (stream-cdr t))))

;(display "(pairs-3 integers integers): ")
;(display-stream (pairs-3 integers integers))

;3.69 (i, j, k)三元组流
(define (square x) (* x x))

(define (pytha i j k)
    (and 
        (< i k)
        (= (square k) (+ (square i) (square j)))))

(define (triples s t u)
    (stream-filter
        (lambda (x) (pytha (car x) (cadr x) (caddr x)))
        (cons-stream
            (list (stream-car s) (stream-car t) (stream-car u))
            (interleave
                (stream-map 
                    (lambda (x) (cons (stream-car s) x))
                    (stream-cdr (pairs t u)))
                (triples
                    (stream-cdr s)
                    (stream-cdr t)
                    (stream-cdr u))))))

 (define first-of-integer-pair 
   (stream-map car (pairs integers integers))) 
  
 (define (triples s t u) 
   (let ((pairs-tu (pairs t u))) ;; compute pairs only *once* 
     (define (rec si i ptu top-i) 
       (cons-stream 
        (cons (stream-car si) (stream-car ptu)) 
        (if (= i (stream-car top-i)) 
            (rec s 1 (stream-cdr ptu) (stream-cdr top-i)) 
            ;; restart s cycle with next ptu 
            (rec (stream-cdr si) (1+ i) ptu top-i)))) 
     (rec s 1 pairs-tu first-of-integer-pair))) 
  
 ;; example: pythagorean triples 
  
 (define triples-integers 
   (triples integers integers integers)) 
  
 (define (pythagorean? a b c) 
   (= (square c) 
      (+ (square a) (square b)))) 
  
 (define pythagorean-triples 
   (stream-filter 
    (lambda (triple) 
      (apply pythagorean? triple)) 
    triples-integers)) 
  
;(display (stream-ref pythagorean-triples 0)) ; (3 4 5) 
;(display (stream-ref pythagorean-triples 1)) ; (6 8 10) 
;(display (stream-ref pythagorean-triples 2)) ; (5 12 13) 
;(display (stream-ref pythagorean-triples 3)) ; (9 12 15) 
;(display (stream-ref pythagorean-triples 4)) ; (8 15 17) 

;(display "(triples integers integers integers): ")
;(display-stream (triples integers integers integers))




;(define (triples s t u)
;  (cons-stream
;   (list
;    (stream-car s)
;    (stream-car t)
;    (stream-car u))
;   (interleave
;    (stream-map
;     (lambda (x) (append (list (stream-car s)) x))
;     (stream-cdr (pairs t u)))
;    (triples
;     (stream-cdr s)
;     (stream-cdr t)
;     (stream-cdr u)))))

;(define pythagorean-triples
;  (stream-filter (lambda (t)
;                   (= (+ (square (car t))
;                         (square (cadr t)))
;                      (square (caddr t))))
;                 (triples integers integers integers) ))

;(display-stream pythagorean-triples)



;3.70
(define (merge-weighted s1 s2 weight)
    (cond 
        ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
            (let ((c1 (stream-car s1)) (c2 (stream-car s2)))
                (cond 
                    ((< (weight c1 c2) (weight c2 c1)) (cons-stream c1 (merge (stream-cdr s1) s2)))
                    ((> (weight c1 c2) (weight c2 c1)) (cons-stream c2 (merge s1 (stream-cdr s2))))
                    (else (cons-stream c1 (merge-weighted (stream-cdr s1) (stream-cdr s2) weight))))))))

(define (weighted-pairs s t weight)
    (cons-stream
        (if (<= (weight (stream-car s)) (weight (stream-car t))) (list (stream-car s) (stream-car t))
            (list (stream-car t) (stream-car s)))
        (interleave
            (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
            (weighted-pairs (stream-cdr s) (stream-cdr t) weight))))

(define weighted-integers (weighted-pairs integers integers (lambda (x) x)))
(display "(weighted-pairs integers integers i<=j order by i+j): ")
(display-stream weighted-integers)

(display-stream (merge-weighted
        weighted-integers
        weighted-integers
        (lambda (x y)
            (- (+ (car x) (cadr x))
                (+ (car y) (cadr y))))))

;a)
(define weighted-integers-2 
    (stream-filter
        (lambda (p)
            (let ((i (car p)) (j (cadr p)))
                (define (q? x) (or (= 0 (remainder x 2)) (= 0 (remainder x 3)) (= 0 (remainder x 5))))
                (or (q? i) (q? j))))
            ;(= i j)))
        (weighted-pairs integers integers (lambda (x) x))))

;b)
(display "(weighted-pairs integers integers i<=j order by 2i+3j+5ij): ")
(display-stream weighted-integers-2)
(display-stream (merge-weighted
        weighted-integers-2
        weighted-integers-2
        (lambda (x y)
            (define (e i j)
                (+ (* 2 i) (* 3 j) (* 5 i j)))
            (- (e (car x) (cadr x))
                (e (car y) (cadr y))))))

;3.71


;3.72






;3.80





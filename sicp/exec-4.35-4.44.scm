(library-directories "..")
(import (modules))

;4.35
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(define (an-integer-between l h)
  (if (= l h) (amb)
    (amb l (an-integer-between (+ l 1) h))))

;4.36
(define (an-integer-start-from n)
  (amb n (an-integer-start-from (+ n 1))))

;i<=j<=k && i^2+j^2=k^2
(define (a-pythagorean-triple-from low)
  (let ((i (an-integer-start-from low)))
    (let ((j (an-integer-start-from i)))
      (let ((k (an-integer-start-from j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))
;反复try again, 只会在最后一个非确定性点k这里反复在无限整数上锁搜寻, 而不会回溯搜寻j和i

(define (a-pythagorean-triple-from-2 low)
  (let ((k (an-integer-start-from low)))
    (let ((i (an-integer-between low k)))
      (let ((j (an-integer-between i k)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))
;amb代表可能的分叉, 分叉的顺序是先k再i再j, i和j的搜寻范围可控, 保证有机会回溯到k


;4.37
(define (a-pythagorean-triple-betwee-4 low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (*j j))))
        (require  (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))
;i^2+j^2<=high^2限制了i j和搜寻范围, 理论上可以提高效率
;比如low=1, high=10, 7 8 9

;4.38
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;[[1,4,3,5,2]]


;4.39
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper) 1)
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;4.40
(define (multiple-dwelling-3)
  (let ((baker (amb 1 2 3 4))
        (cooper (amb 2 3 4 5)))
    (let ((fletcher (not-neighbour cooper 1)))
      (require (not (= fletcher 5)))
      (require (not (= fletcher 1)))
      (let ((miller (upstairs cooper 1))
            (smith (not-neighbour fletcher 1)))
        (require
          (distinct? (list baker cooper fletcher miller smith)))
        (list (list 'baker baker)
              (list 'cooper cooper)
              (list 'fletcher fletcher)
              (list 'miller miller)
              (list 'smith smith))))))

(define (upstairs x n)
  (amb (+ x n)))

(define (not-neighbour x n)
  (amb (+ x (+ n 1)) (- x (+ n 1))))

;4.41
;先实现排列组合
;再用filter去过滤

;4.42
(define (xor p q) (if p (not q) q))
(define (lier)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (john (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require (xor (= kitty 2) (= betty 3)))
    (require (xor (= john 2) (= ethel 1)))
    (require (xor (= ethel 5) (= john 3)))
    (require (xor (= kitty 2) (= mary 4)))
    (require (xor (= mary 4) (= betty 1)))
    (require
      (distinct? (list betty ethel john kitty mary)))
    (list 
      (list 'betty betty)
      (list 'ethel ethel)
      (list 'john john)
      (list 'kitty kitty)
      (list 'mary mary))))


;4.43

;4.44














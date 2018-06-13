(library-directories "..")
(import (modules))

(load "exec-4.27-base.scm")

;4.27
;(define count 0)
;(define (id x) (set! count (+ count 1)) x)
;(define w (id (id 10)))
;交替eval w 和 count
;eval w时会执行id函数的body部分, 既count+1, body部分最后的语句是参数x既(id 10), 这时候返回的是trunked (id 10), 既w被赋值为trunked (id 10), 并没有执行
;除非在把w作为函数的参数执行, 比如(display w), 这个时候w里的trunked (id 10)才会被真正求值

;4.28
;eval
;   (proc arg-a arg-b arg-c)

;4.29
;(eval '(define count 0) the-global-environment)
;(eval '(define (id x) (set! count (+ count 1)) (display count) x) the-global-environment)
;(eval '(define (square x) (* x x)) the-global-environment)
;(eval '(display (square (id 10))) the-global-environment)
;参数x: trunked (id 10)执行过一次后会被记忆, 作为乘数的时候直接获取记忆的值, 所以count只会被+1

;4.30
(eval
  '(define (for-each proc items)
    (if (null? items) 'done
      (begin (proc (car items))
        (for-each proc (cdr items)))))
the-global-environment)

(eval
  '(for-each 
      (lambda (x) (newline) (display x))
      (list 11 22 33))
the-global-environment)




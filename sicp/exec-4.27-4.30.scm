(library-directories "..")
(import (modules))

(load "exec-4.27-base.scm")

;4.27
;(define count 0)
;(define (id x) (set! count (+ count 1)) x)
;(define w (id (id 10)))
;交替eval w 和 count
;因为id使用延时求值, 每次eval w的时候都会执行两次id

;4.28
eval
	(proc arg-a arg-b arg-c)
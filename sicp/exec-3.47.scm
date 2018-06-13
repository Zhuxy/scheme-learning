(library-directories "..")
(import (modules))


;3.47
;Semaphore
(define (make-semaphore n)
    (let ((s n)
          (mutex (make-mutex)))
        (define (dispatch m)
            (cond 
                ((eq? m 'acquire)
                    (mutex 'acquire)
                    (if (= s 0)
                        (begin (mutex 'release) (dispatch 'acquire))
                        (begin
                            (set! s (- s 1))
                            (mutex 'release)
                            #t)))
                ((eq? m 'release)
                    (mutex 'acquire)
                    (set! s (+ s 1))
                    (mutex 'release)
                    #t)))
        dispatch))


(define (make-semaphore-1 n)
    (let ((cell (list #f)))
        (define (dispatch m)
            (cond 
                ((eq? m 'acquire)
                    (if (test-and-set! cell)
                        (if (= s 0) 
                            (begin (set-car! cell #f) (dispatch 'acquire))
                            (begin
                                (set! s (- s 1))
                                (set-car! cell #f)
                                #t))
                        (dispatch 'acquire)))
                ((eq? m 'release)
                    (if (test-and-set! cell)
                        (begin
                            (set! s (+ s 1))
                            (set-car! cell #f)
                            #t)
                        (dispatch 'release)))))
        dispatch))


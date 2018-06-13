(define (add-it lst)
    (if (null? (cdr lst)) lst
        (append (list (car lst) '+) (add-it (cdr lst)))))

(display (add-it '(1 2 3 4)))
;(display (add-it (cdr '(1 2))))
;(display (append (list (car '(1 2)) '+) '(2)))
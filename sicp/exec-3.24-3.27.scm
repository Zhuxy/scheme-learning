(library-directories "..")
(import (modules))

(define (assoc key records)
    (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
    (let ((local-table (list `*table*)))
        (define (lookup key-1 key-2)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record (cdr record) #f))
                    #f)))
        (define (insert! key-1 key-2 value)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record
                            (set-cdr! record value)
                            (set-cdr! subtable
                                (cons 
                                    (cons key-2 value)
                                    (cdr subtable)))))
                    (set-cdr! local-table
                        (cons (list key-1
                                    (cons key-2 value))
                                (cdr local-table)))))
            `ok)
        (define (dispatch m)
            (cond ((eq? m `lookup-proc) lookup)
                ((eq? m `insert-proc) insert!)
                (else (error `dispatch "Unknow operation -- TABLE" m))))
        dispatch))

(define t (make-table))
(define put (t `insert-proc))
(define get (t `lookup-proc))
(put `math `+ 1)
(put `math `- 2)
(put `english `a 1)
(put `english `b 2)
(displayn "get math +: "(get `math `+))
(displayn "get english b: "(get `english `b))

;3.24
(define (make-table-1 same-key?)
    (let ((local-table (list `*table*)))
        (define (assoc key records)
        (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
        (define (lookup key-1 key-2)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record (cdr record) #f))
                    #f)))
        (define (insert! key-1 key-2 value)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record
                            (set-cdr! record value)
                            (set-cdr! subtable
                                (cons 
                                    (cons key-2 value)
                                    (cdr subtable)))))
                    (set-cdr! local-table
                        (cons (list key-1
                                    (cons key-2 value))
                                (cdr local-table)))))
            `ok)
        (define (dispatch m)
            (cond ((eq? m `lookup-proc) lookup)
                ((eq? m `insert-proc) insert!)
                (else (error `dispatch "Unknow operation -- TABLE" m))))
        dispatch))

(define t (make-table-1 equal?))
(define put (t `insert-proc))
(define get (t `lookup-proc))
(put `math `(1 + 2) 3)
(put `math `(5 - 1) 4)
(put `english `a 1)
(put `english `b 2)
(displayn "get math +: "(get `math `(1 + 2)))
(displayn "get english b: "(get `english `b))

;3.25
;3.26
;3.27















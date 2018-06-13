(library-directories "..")
(import (modules))

(define (make-queue) (cons '() '()))

(define (front-prt queue) (car queue))

(define (rear-prt queue) (cdr queue))

(define (empty-queue? queue)
    (null? (front-prt queue)))

(define (set-front-prt! queue item) (set-car! queue item))

(define (set-rear-prt! queue item) (set-cdr! queue item))

(define (front-queue queue)
    (if (empty-queue? queue) (error 'front-queue "empty queue!!!")
        (car (front-prt queue))))

(define (insert-queue! queue item)
    (let ((new-pair (cons item '())))
        (cond ((empty-queue? queue)
                (set-front-prt! queue new-pair)
                (set-rear-prt! queue new-pair)
                queue)
            (else
                (set-cdr! (rear-prt queue) new-pair)
                (set-rear-prt! queue new-pair)
                queue))))

(define (delete-queue! queue)
    (cond ((empty-queue? queue) (error 'delete-queue! "cannot delete empty queue"))
        (else
            (set-front-prt! queue (cdr (front-prt queue)))
            queue)))


;3.21
(define (print-queue queue)
    (define (print-it front)
        (cond ((null? front) (display ""))
            (else (display (car front))
                (display " ")
                (print-it (cdr front)))))
    (let ((front (front-prt queue)))
        (display "queue: ( ")
        (print-it front)
        (display ")")
        (newline)))

(define q (make-queue))
(print-queue q)
(insert-queue! q 'a)
(print-queue q)
(insert-queue! q 'b)
(print-queue q)
(delete-queue! q)
(print-queue q)
(delete-queue! q)
(print-queue q)

;3.22
(define (make-queue1)
    (define queue (cons '() '()))
    (define (front-prt queue) (car queue))
    (define (rear-prt queue) (cdr queue))
    (define (empty-queue? queue)
        (null? (front-prt queue)))
    (define (set-front-prt! queue item) (set-car! queue item))
    (define (set-rear-prt! queue item) (set-cdr! queue item))
    (define (front-queue)
        (if (empty-queue? queue) (error 'front-queue "empty queue!!!")
            (car (front-prt queue))))
    (define (insert-queue! item)
        (let ((new-pair (cons item '())))
            (cond ((empty-queue? queue)
                    (set-front-prt! queue new-pair)
                    (set-rear-prt! queue new-pair)
                    queue)
                (else
                    (set-cdr! (rear-prt queue) new-pair)
                    (set-rear-prt! queue new-pair)
                    queue))))
    (define (delete-queue!)
        (cond ((empty-queue? queue) (error 'delete-queue! "cannot delete empty queue"))
            (else
                (set-front-prt! queue (cdr (front-prt queue)))
                queue)))
    (define (dispatch m)
        (cond 
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)))
    dispatch)

(define q2 (make-queue1))
((q2 'insert-queue!) 'a)
((q2 'insert-queue!) 'b)
(display ((q2 'front-queue)))(newline)
((q2 'delete-queue!))
(display ((q2 'front-queue)))(newline)

;3.23
(define (make-deque)
    (cons '() '()))

(define (empty-deque? deque)
    (null? (car deque)))

(define (front-deque deque)
    (if (empty-deque? deque) (error 'front-deque "empty deque!!!")
        (car (car deque))))

(define (rear-deque deque)
    (if (empty-deque? deque) (error 'rear-deque "empty deque!!!")
        (cadr (cdr deque))))

(define (front-insert-deque! deque item)
    (if (empty-deque? deque)
        (let ((front (list '() item '())))
                (set-car! deque front)
                (set-cdr! deque front)
                deque)
        (let* ((old-front (car deque))
                (front (list '() item old-front)))
                (set-car! old-front front)
                (set-car! deque front)
                deque)))

(define (rear-insert-deque! deque item)
    (if (empty-deque? deque)
        (let ((rear (list '() item '())))
            (set-car! deque rear)
            (set-cdr! deque rear)
            deque)
        (let ((rear (list (cdr deque) item '()))
	      (old (cdr deque)))
            (set-cdr! (cdr old) (cons rear '()))
	    (set-cdr! deque rear)
            deque)))

(define (front-delete-deque! deque)
    (cond ((empty-deque? deque) (error 'front-delete-deque! "cannot delete empty deque"))
        (else
            (let ((front (caddr (car deque))))
                (set-car! deque front)
                deque))))
                
(define (rear-delete-deque! deque)
    (cond ((empty-deque? deque) (error 'rear-delete-deque! "cannot delete empty deque"))
        (else
            (let* ((old-rear (cdr deque))
                    (rear (car old-rear)))
                (set-cdr! (cdr rear) '())
                (set-cdr! deque rear)
                deque))))
                
(define (print-deque deque)
    (define (print-it front)
        (cond ((null? front ) (display ""))
            (else
	        (display (cadr front))
                (display " ")
                (print-it (caddr front)))))
    (let ((front (car deque)))
        (display "deque: ( ")
        (print-it front)
        (display ")")
        (newline)))

(define d1 (make-deque))(print-deque d1)

(front-insert-deque! d1 'a)(print-deque d1)

(front-insert-deque! d1 'b)(print-deque d1)

(front-delete-deque! d1)(print-deque d1)

(front-delete-deque! d1)(print-deque d1)

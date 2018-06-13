(library-directories "..")
(import (modules))

;3.5
(define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
        (cond ((= trials-remaining 0)
                (/ trials-passed trials))
            ((experiment)
                (iter (- trials-remaining 1) (+ trials-passed 1)))
            (else
                (iter (- trials-remaining 1) trials-passed))))
    (iter trials 0))

(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (random range))))

;随机定义一个谓词所需的圆, 落在矩形内
;圆的描述结构(x y r)
(define (make-round x1 x2 y1 y2)
    (let ((x (random-in-range x1 x2))
            (y (random-in-range y1 y2)))
        (list x y (min (- x x1) (- x2 x) (- y y1) (- y2 y)))))

(displayn "make-round: " (make-round 0 10 0 10))

(define (square x) (* x x))

(define (test-point-round point target-round)
    (let ((x0 (car point)) 
            (y0 (cdr point))
            (x1 (car target-round))
            (y1 (cadr target-round))
            (r (caddr target-round)))
        (<= (+ (square (- x0 x1)) (square (- y0 y1))) 
            (square r))))

(define (estimate-integral x1 x2 y1 y2)
    (let*
        ((target-round (make-round x1 x2 y1 y2))
            (experiment 
                (lambda ()
                    ;point (x . y)
                    (let ((random-point (cons (random-in-range x1 x2) (random-in-range y1 y2))))
                        (test-point-round random-point target-round))))
            ;试验的次数=矩形面积一半?
            (rect-area (* (- x2 x1) (- y2 y1)))
            (trials (round (/ (* rect-area 3) 4)))
            (round-area (* (monte-carlo trials experiment) rect-area)))
        ;计算pi
        (/ round-area (square (caddr target-round)))
    )
)

(displayn "estimate-integral: " (estimate-integral 0 1000 0 1000))

(define (random-init) (random-in-range 0 100))

(define (random-update x)
    (remainder (+ (* 13 x) 5) 24))

;3.6
(define (rand)
    (let ((x (random-init)))
        (lambda (m)
            (cond ((equal? m 'generate) 
                        (begin
                            (set! x (random-update x))
                            x))
                    ((equal? m 'reset)
                        (lambda (val)
                            (set! x val)))
                (else (error 'rand "wrong command"))))))













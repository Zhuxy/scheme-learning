(library-directories "..")
(import (modules))

(define (make-machine register-names ops controller-text)
    (let ((machine (make-new-machine)))
        (for-each 
            (lambda (register-name) ((machine 'allocate-register) register-name))
            register-names)
        ((machine 'install-operations) ops)
        ((machine '7) (assemble controller-text machine))
        machine))

(define (make-register name)
    (let ((contents '*unassigned*))
        (lambda (message) 
            (cond ((eq? message 'get) contents)
                ((eq? message 'set) (lambda (value) (set! contents value)))
                (else (error "UNKNOW request -- REGISTER" message)))
        )))

(define (get-contents register)
    (register 'get))

(define (set-contents! register value)
    ((register 'set) value))

;test register
(define reg-a (make-register 'a))
(set-contents! reg-a 1)
(displayn "get from reg-a: " (get-contents reg-a))

(define (make-stack)
    (let ((stack '()))
        (define (push v) (set! stack (cons v stack)))
        (define (pop)
            (if (null? stack)
                (error "EMPTY STACK -- POP" "")
                (let ((top (car stack)))
                    (set! stack (cdr stack) )
                    top)))
        (define (initialize) (set! stack '()) 'done)
        (lambda (message)
            (cond ((eq? message 'pop) (pop))
                ((eq? message 'push) (lambda (value) (push value)))
                ((eq? message 'initialize) (initialize))
                (else (error "UNKNOW request -- STACK" message))))))

(define (pop stack)
    (stack 'pop))

(define (push stack value)
    ((stack 'push) value))

;test stack
(define stack (make-stack))
(push stack 1)
(push stack 2)
(displayn "pop from stack: " (pop stack))
(displayn "pop from stack: " (pop stack))


(define (start machine)
    (machine 'start))

(define (get-register-contents machine register-name) 
    (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value) 
    (set-contents! (get-register machine register-name value))
    'done)

(define  (make-new-machine)
    (let ((pc  (make-register 'pc))     ;下一条执行的指令
            (flag (make-register 'flag))    ;记录test的结果
            (stack (make-stack))
            (the-instruction-sequence ' ())) 
        (let ((the-ops (list (list 'initialize-stack    ; 指令列表是个associate table ((name value) (name value))
                                (lambda () (stack 'initialize)))))
                (register-table (list (list 'pc pc) (list 'flag flag)))) ; 寄存器列表也是associate table
            (define (allocate-register name)
                (if (assoc name register-table)    ; assoc在关联表里查询key对应的那一个序对
                    (error "Multiply defined registers " name)
                    (set! register-table 
                        (cons (list name (make-register name)) register-table)))
                'register-allocated)
            
            (define (lookup-register name)
                (let ((val (assoc name register-table)))
                    (if val
                        (cadr val)
                        (error"Unknown register"name))))

            (define (execute)
                (let ((insts (get-contents pc)))
                    (if (null? insts)
                        'done
                        (begin
                            ((instruction-execution-proc (car insts)))
                            (execute)))))
            
            (define (dispatch message) 
                (cond ((eq? message 'start)
                            (set-contents! pc the-instruction-sequence)
                            (execute))
                    ((eq? message '7)
                            (lambda  (seq) (set! the-instruction-sequence seq)))
                    ((eq? message 'allocate-register) allocate-register)
                    ((eq? message 'get-register) lookup-register)
                    ((eq? message 'install-operations)
                            (lambda  (ops) (set! the-ops (append the-ops ops))))
                    ((eq? message 'stack) stack)
                    ((eq? message 'operations) the-ops)
                    (else (error "Unknown request --machine" message))))
            dispatch)))

(define (get-register machine reg-name)
    ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
    (extract-labels controller-text
        (lambda (insts labels)
            (update-insts! insts labels machine)
            )))

(define (extract-labels text receive)
    (if (null? text)
        (receive '() '())
        (extract-labels (cdr text)
            (lambda (insts labels)
                (let ((next-inst (cdr text)))
                    (if (symbol? next-inst)
                        (if (assoc next-inst labels)                ; 5.8
                            (error "Duplicated label -- ASSEMBLE" next-inst)
                            (receive insts
                                (cons (make-label-entry next-inst insts) labels)))
                        (receive (cons (make-instruction next-inst) insts) labels)))))))

(define (update-insts! insts labels machine)
    (let ((pc (get-register machine 'pc))
            (flag (get-register 'flag))
            (stack (machine 'stack))
            (ops (machine 'operations)))
        (for-each
            (lambda (inst)
                (set-instruction-execution-proc!
                    inst
                    (make-execution-procedure
                        (instruction-text inst)
                        labels machine pc flag stack ops)))
            insts)))

(define (make-instruction text)
    (cons text '()))

(define (instruction-text inst)
    (car inst))

(define (instruction-execution-proc inst)
    (cdr inst))

(define (set-instruction-execution-proc! inst proc)
    (set-cdr! inst proc))

(define (make-label-entry label-name insts)
    (cons label-name insts))

(define (lookup-label labels label-name)
    (let ((val (assoc label-name labels)))
        (if val
            (cdr val)
            (error "Undefinded label -- ASSEMBLE" label-name))))
(load "./chap5/regsim.scm")

;; b
(define (make-save inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name)))
      (lambda ()
        (push stack (cons reg-name (get-contents reg)))
        (advance-pc pc)))))

(define (make-restore inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name)))
      (lambda ()
        (let ((head-of-stack (pop stack)))
          (if (eq? (car head-of-stack) reg-name)
            (set-contents! reg (cdr head-of-stack))
            (error "Wrong register name - RESTORE" reg-name)))
        (advance-pc pc)))))

;; c
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name)
                ((machine 'allocate-stack-for-register) register-name))
              register-names)
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack '())
        (the-instruction-sequence '()))
    (let ((the-ops
            (list (list 'initialize-stack
                        (map (lambda (named-stack) ((cdr named-stack) 'initialize))
                             stack)
                        (lambda () (stack 'initialize)))
                  (list 'print-stack-statistics
                        (lambda () (stack 'print-statistics)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (set! register-table
            (cons (list name (make-register name))
                  register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
            'done
            (begin
              ((instruction-execution-proc (car insts)))
              (execute)))))
      (define (allocate-stack-for-register register-name)
        (set! stack (cons (cons register-name (make-stack)) stack)))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'allocate-stack-for-register)
               allocate-stack-for-register)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      ;; named-stack (name (stack))
      (let ((named-stack (assoc (stack-inst-reg-name inst) stack)))
        (push (cdr named-stack) (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      ;; named-stack (name (stack))
      (let ((named-stack (assoc (stack-inst-reg-name inst) stack)))
        (set-contents! reg (pop (cdr named-stack))))
      (advance-pc pc))))

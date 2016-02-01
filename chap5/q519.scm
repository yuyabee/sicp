(load "./chap5/q518.scm")

(define (list-index lst e)
	(define (iter lst res)
		(cond ((null? lst) #f)
					((eq? (car lst) e) res)
					(else (iter (cdr lst) (+ 1 res)))))
	(iter lst 0))

(define (make-new-machine)
	(let ((pc (make-register 'pc))
				(flag (make-register 'flag))
				(stack (make-stack))
				(the-instruction-sequence '())
				(instruction-counter 0)
				(trace #f)
				(current-line '())
				(breakpoints '())
				;; continuation
				(proceed '()))
		(let ((the-ops
						(list (list 'initialize-stack
												(lambda () (stack 'initialize)))
									(list 'print-stack-statistics
												(lambda () (stack 'print-statistics)))))
					(register-table
						(list (list 'pc pc)
									(list 'flag flag))))
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
							(if (symbol? (car insts))
								(begin
									(set! current-line (cons (car insts) 0))
									(if trace
										(begin
											(display "TRACE:  ")
											(display (car insts))
											(newline)))
									(advance-pc pc)
									(execute))
								(begin
									(set! (cdr current-line) (+ (cdr current-line) 1))
									(set! instruction-counter (+ 1 instruction-counter))
									(set! proceed (lambda ()
																	(if trace
																		(begin (display "TRACE:    ")
																					 (display (instruction-text (car insts)))
																					 (newline)))
																	((instruction-execution-proc (car insts)))
																	(execute)))
									(if (member current-line breakpoints)
										'BREAK-POINT!
										(proceed))))))))
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
							((eq? message 'count-instruction)
							 (let ((res instruction-counter))
								 (set! instruction-counter 0)
								 res))
							((eq? message 'reset-instruction-counter) (set! instruction-counter 0))
							((eq? message 'trace-on) (set! trace #t))
							((eq? message 'trace-off) (set! trace #f))
							((eq? message 'add-breakpoint)
							 (lambda (label n)
								 (set! breakpoints (cons (cons label n) breakpoints))))
							((eq? message 'proceed-machine)
							 (proceed))
							((eq? message 'cancel-breakpoint)
							 (lambda (label n)
								 (set! breakpoints
									 (filter
										 (lambda (x) (not (equal? (list label n) x)))
										 break-points))))
							((eq? message 'cancel-all-breakpoints) (set! breakpoints '()))
							(else (error "Unknown request -- MACHINE" message))))
			dispatch)))

(define (set-breakpoint machine label n)
	((machine 'add-breakpoint) label n))

(define (proceed-machine machine)
	(machine 'proceed-machine))

(define (cancel-breakpoint machine label n)
	((machine 'cancel-breakpoint) label n))

(define (cancel-all-breakpoints machine)
	(machine 'cancel-all-breakpoints))

(load "./chap5/q517.scm")

(define (make-register name)
	(let ((reg-name name)
				(contents '*unassigned*)
				(trace #f))
		(define (show-trace-register value)
			(display "TRACE-REGISTER: ")
			(display "name=")
			(display reg-name)
			(display " old-value=")
			(display contents)
			(display " new-value=")
			(display value)
			(newline))
		(define (dispatch message)
			(cond ((eq? message 'get) contents)
						((eq? message 'set)
						 (lambda (value) (if trace (show-trace-register value))
							 (set! contents value)))
						((eq? message 'trace-on) (set! trace #t))
						((eq? message 'trace-off) (set! trace #f))
						(else
							(error "Unknown request -- REGISTER" message))))
		dispatch))

(define (set-register-trace-flag machine register-name flag)
	((get-register machine register-name) flag))

(define fact-machine
	(make-machine
		'(continue val n)
		(list (list '= =) (list '- -) (list '* *))
		'(start
			 (assign continue (label fact-done))
			 fact-loop
			 (test (op =) (reg n) (const 1))
			 (branch (label base-case))
			 (save continue)
			 (save n)
			 (assign n (op -) (reg n) (const 1))
			 (assign continue (label after-fact))
			 (goto (label fact-loop))
			 after-fact
			 (restore n)
			 (restore continue)
			 (assign val (op *) (reg n) (reg val))
			 (goto (reg continue))
			 base-case
			 (assign val (const 1))
			 (goto (reg continue))
			 fact-done)))

(fact-machine 'trace-on)

(define (factorial n)
	(set-register-trace-flag fact-machine 'val 'trace-on)
	(set-register-trace-flag fact-machine 'n 'trace-on)
	(set-register-contents! fact-machine 'n n)
	(start fact-machine)
	(get-register-contents fact-machine 'val))

(load "./chap5/regsim.scm")

(define (make-new-machine)
	(let ((pc (make-register 'pc))
				(flag (make-register 'flag))
				(stack (make-stack))
				(the-instruction-sequence '())
				(instruction-counter 0)
				(trace #f))
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
									(if trace
										(begin
											(display "TRACE:  ")
											(display (car insts))
											(newline)))
									(advance-pc pc)
									(execute))
								(begin
									(if trace
										(begin (display "TRACE:    ")
													 (display (instruction-text (car insts)))
													 (newline)))
									(set! instruction-counter (+ 1 instruction-counter))
									((instruction-execution-proc (car insts)))
									(execute)))))))
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
							((eq? message 'reset-instruction-counter)
							 (set! instruction-counter 0))
							((eq? message 'trace-on) (set! trace #t))
							((eq? message 'trace-off) (set! trace #f))
							(else (error "Unknown request -- MACHINE" message))))
			dispatch)))

(define (extract-labels text receive)
	(if (null? text)
		(receive '() '())
		(extract-labels
			(cdr text)
			(lambda (insts labels)
				(let ((next-inst (car text)))
					;; if next-inst is a symbol that is a label
					(if (symbol? next-inst)
						(receive (cons next-inst insts) ;; added
										 ; make-label-entry is the same as cons
										 (cons (make-label-entry next-inst
														;; to prompt the label name, label needs to contain
														;; the name of the label
																						 (cons next-inst insts)) ;; added
													 labels))
						(receive (cons (make-instruction next-inst)
													 insts)
										 labels)))))))

(define (update-insts! insts labels machine)
	(let ((pc (get-register machine 'pc))
				(flag (get-register machine 'flag))
				(stack (machine 'stack))
				(ops (machine 'operations)))
		(for-each
			(lambda (inst)
				;; setting executable procedure to cdr of inst
				(if (symbol? inst) ;; added
					'label
					(set-instruction-execution-proc! 
						inst
						(make-execution-procedure
							(instruction-text inst) labels machine
							pc flag stack ops))))
			insts)))

(define (make-execution-procedure inst labels machine
																	pc flag stack ops)
	;; why not use (instruction-text inst) instead of (car inst)
	(cond ((eq? (car inst) 'assign)
				 (make-assign inst machine labels ops pc))
				((eq? (car inst) 'test)
				 (make-test inst machine labels ops flag pc))
				((eq? (car inst) 'branch)
				 (make-branch inst machine labels flag pc))
				((eq? (car inst) 'goto)
				 (make-goto inst machine labels pc))
				((eq? (car inst) 'save)
				 (make-save inst machine stack pc))
				((eq? (car inst) 'restore)
				 (make-restore inst machine stack pc))
				((eq? (car inst) 'perform)
				 (make-perform inst machine labels ops pc))
				((eq? (car inst) 'label) ;; added
				 (car  inst))
				(else (error "Unknown instruction type -- ASSEMBLE"
										 inst))))

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

(define (factorial n)
	(set-register-contents! fact-machine 'n n)
	(start fact-machine)
	(format #t "factorial ~2d => ~4d\n"
					n
					(get-register-contents fact-machine 'val)))

(load "./chap5/regsim.scm")

(define factorial-machine
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

(define (enumerate-interval low high)
	(if (> low high)
		()
		(cons low (enumerate-interval (+ low 1) high))))

(define (analyze-fact x)
	(map (lambda (n)
				 (set-register-contents! factorial-machine 'n n)
				 (start factorial-machine)
				 (list
					 (get-register-contents factorial-machine 'n)
					 (get-register-contents factorial-machine 'val)
					 ((factorial-machine 'stack) 'print-statistics)))
			 (enumerate-interval 1 x)))

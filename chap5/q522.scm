(load "./chap5/q517.scm")

(define (append x y)
	(if (null? x)
		y
		(cons (car x) (append (cdr x) y))))

(define append-machine
	(make-machine
		'(continue x y val tmp)
		(list (list 'null? null?)
					(list 'cons cons)
					(list 'car car)
					(list 'cdr cdr))
		'(start
				(assign continue (label append-done))
			append-loop
				(test (op null?) (reg x))
				(branch (label null))
				(save continue)
				(assign continue (label consing))
				(save x)
				(assign x (op cdr) (reg x))
				(goto (label append-loop))
			null
				(assign val (reg y))
				(goto (reg continue))
			consing
				(restore x)
				(restore continue)
				(assign tmp (op car) (reg x))
				(assign val (op cons) (reg tmp) (reg val))
				(goto (reg continue))
			append-done)))

(define (append! x y)
	(set-cdr! (last-pair x) y)
	x)

(define (last-pair x)
	(if (null? (cdr x))
		x
		(last-pair (cdr x))))

(define append!-machine
 (make-machine
	'(continue x y cdr-x tmp)
	(list (list 'null? null?)
				(list 'set-cdr! set-cdr!)
				(list 'cdr cdr))
	'(start
			(assign continue (label append!-done))
			(save x)
			(assign cdr-x (reg x))
		last-pair
			(assign x (reg cdr-x))
			(assign cdr-x (op cdr) (reg cdr-x))
			(test (op null?) (reg cdr-x))
			(branch (label null))
			(goto (label last-pair))
		null
			;; trying to use the side effect of set-cdr!, so assigning tmp to get the
			;; value returned by set-cdr!
			(assign tmp (op set-cdr!) (reg x) (reg y))
			(goto (label append!-done))
		append!-done
			(restore x))))

(define x (list 'a 'b))
(define y (list 'c 'd))

(append-machine 'trace-on)
(set-register-contents! append-machine 'x x)
(set-register-contents! append-machine 'y y)
(start append-machine)

(append!-machine 'trace-on)
(set-register-contents! append!-machine 'x x)
(set-register-contents! append!-machine 'y y)
(start append!-machine)

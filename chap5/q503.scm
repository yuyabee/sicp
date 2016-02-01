(controller
	sqrt-loop
		(assign guess (const 1.0))
		(assign x (op read))
	iter
		(test (op good-enough?) (reg guess) (reg x))
		(branch (label sqrt-done))
		(assign guess (op improve) (reg guess) (reg x))
		(goto (label iter))
	sqrt-done
		(perform (op print) (reg guess))
		(goto (label sqrt-loop)))

(controller
	sqrt-loop
		(assign guess (const 1.0))
		(assign x (op read))
	iter
		(assign t1 (op *) (reg guess) (reg guess))
		(assign t2 (op -) (reg x) (reg t1))
		(assign t3 (op abs) (reg t2))
		(test (op <) (reg t3) (const 0.001))
		(branch (label sqrt-done))
		(assign t4 (op /) (reg x) (reg guess))
		(assign guess (op average) (reg t4) (reg guess))
		(goto (label iter))
	sqrt-done
		(perform (op print) (reg guess))
		(goto (label sqrt-loop)))

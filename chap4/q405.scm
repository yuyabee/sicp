(load "./chap4/base.scm")

(define (expand-clauses clauses)
	(if (null? clauses)
		#f
		(let ((first (car clauses))
					(rest (cdr clauses)))
			(if (cond-else-clause? first)
				(if (null? rest)
					(sequence->exp (cond-actions first))
					(error "ELSE clause isn't last -- COND->IF"
								 clauses))
				(make-if (cond-predicate first)
								 (let ((action (cond-actions first))
											 (predicate (cond-predicate first)))
									 (if (eq? (car action) '=>)
										 (list (cadr action) predicate)
										 (sequence->exp action)))
								 (expand-clauses rest))))))

(define (make-table)
	(let ((table (list '*table*)))
		(define (lookup key-list)
			(lookup-iter key-list table))
		(define (lookup-iter key-list table)
			(if (null? key-list)
				#f
				(let ((sub-table (assoc (car key-list) table)))
					(if sub-table
						(if (null? key-list)
							(cdr sub-table)
							(lookup-iter (cdr key-list) sub-table))
						#f))))
		(define (insert! key-list value)
			(insert-iter key-list value table))
		(define (insert-iter! key-list value table)
			(if (null? key-list)
				#f
				(let ((sub-table (assoc (car key-list) (cdr table))))
					(if sub-table
						(if (null? (cdr key-list))
							(set-cdr! sub-table value)
							(insert-iter! (cdr key-list) value sub-table))
						(set-cdr! table
											(cons (insert-iter key-list value)
														(cdr table))))))
			'ok)
		))

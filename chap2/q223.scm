(define (for-each proc items)
	(if (null? items)
		#f
		(and (proc (car items)) (for-each proc (cdr items)))))

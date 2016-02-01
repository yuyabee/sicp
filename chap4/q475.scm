(load "./chap4/query-system.scm")

(define unique-query car)

(define (uniquely-asserted operands frame-stream)
	(stream-flatmap
		(lambda (frame)
			(let ((result (qeval (unique-query operands)
													 (singleton-stream frame))))
				(if (and (not (stream-null? result))
								 (stream-null? (stream-cdr result)))
					result
					the-empty-stream)))
		frame-stream))

;; (put 'unique 'qeval uniquely-asserted)

(define (initialize-data-base rules-and-assertions)
	(define (deal-out r-and-a rules assertions)
		(cond ((null? r-and-a)
					 (set! THE-ASSERTIONS (list->stream assertions))
					 (set! THE-RULES (list->stream rules))
					 'done)
					(else
						(let ((s (query-syntax-process (car r-and-a))))
							(cond ((rule? s)
										 ;; it modifies the global table of rules
										 (store-rule-in-index s)
										 (deal-out (cdr r-and-a)
															 (cons s rules)
															 assertions))
										(else
											;; it modifies the global table of assertions
											(store-assertion-in-index s)
											(deal-out (cdr r-and-a)
																rules
																(cons s assertions))))))))
	(let ((operation-table (make-table)))
		(set! get (operation-table 'lookup-proc))
		(set! put (operation-table 'insert-proc!)))
	(put 'and 'qeval conjoin)
	(put 'or 'qeval disjoin)
	(put 'not 'qeval negate)
	(put 'lisp-value 'qeval lisp-value)
	(put 'always-true 'qeval always-true)
	(put 'unique 'qeval uniquely-asserted)
	(deal-out rules-and-assertions '() '()))

(initialize-data-base microshaft-data-base)

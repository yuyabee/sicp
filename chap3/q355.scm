; first, (second + third), (third + forth) ...
(define (partial-sums stream)
	(cons-stream (stream-car stream)
							 (add-stream (stream-cdr stream)
													 (partial-sums stream))))

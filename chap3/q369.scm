(load "./chap3/stream.scm")

(define (triples s t u)
	(cons-stream
		(list (stream-car s) (stream-car t) (stream-car u))
		(interleave
			(stream-map (lambda (p) (cons (stream-car s) p))
									(stream-cdr (pairs t u)))
			(triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define pythagorean-stream (stream-filter
														 (lambda (t)
															 (= (+ (square (car t)) (square (cadr t)))
																	(square (caddr t))))
														 (triples integers integers integers)))

(display-stream pythagorean-stream)

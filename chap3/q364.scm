(load "./chap3/stream.scm")

(define (stream-limit stream tolerance)
	(let ((a (stream-car stream))
				(b (stream-car (stream-cdr stream))))
		(if (< (abs (- a b)) tolerance)
			b
			(stream-limit (stream-cdr stream) tolerance))))

(define (sqrt x tolerance)
	(stream-limit (sqrt-stream x) tolerance))

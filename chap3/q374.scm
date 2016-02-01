(load "./chap3/stream.scm")

(define (make-zero-crossings input-stream last-value)
	(cons-stream
		(sign-change-detector (stream-car input-stream) last-value)
		(make-zero-crossings (stream-cdr input-stream)
												 (stream-car input-stream))))

(define zero-crossing (make-zero-crossings sense-data 0))

(define zero-crossing (stream-map sign-change-detector sense-data
																	(cons-stream 0 sense-data)))

(define (sign-change-detector current last)
	(cond ((and (> last 0) (< current 0)) -1)
				((and (< last 0) (> current 0)) 1)
				(else 0)))

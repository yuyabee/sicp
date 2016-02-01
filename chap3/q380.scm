(load "./chap3/stream.scm")

(define (RLC R L C dt)
 (lambda (vC0 iL0)
	(define dvC (scale-stream iL (/ 1 C)))
	(define diL (add-stream (scale-stream vC (/ 1 L))
													(scale-stream iL (- (/ R L)))))
	(define iL (integral (delay diL) iL0 dt))
	(define vC (integral (delay dvC) vC0 dt))
	; i have no idea why it is necessary
	(stream-map (lambda (vCn iLn) (cons vCn iLn)) vC iL)))

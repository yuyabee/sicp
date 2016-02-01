(define (iterative-improve enough? improve)
	(lambda (guess)
		(define (iter guess)
			(if (enough? guess)
				guess
				(iter (improve guess))))
		(iter guess)))

(define (sqrt x)
	(define (good-enough? guess)
		(< (abs (- (square guess) x)) 0.001))
	(define (improve guess)
		(average guess (/ x guess)))
	((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f first-guess)
 (define (close-enough? guess)
	(< (abs (- guess (f guess))) 0.00001))
 (f ((iterative-improve close-enough? f) first-guess)))

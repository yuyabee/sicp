(load "./q143.scm")

(define dx 0.00001)

(define (smooth f)
 (lambda (x)
	(/ (+
			(f x)
			(f (+ x dx))
			(f (- x dx)))
	 3)))

(define (n-fold-smooth f n)
 (repeated smooth n) f)

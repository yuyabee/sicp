(load "./q137.scm")
(load "./base.scm")

(define (tan-cf x k)
	(cont-frac-i
		(lambda (i) (if (= i 1) x (- (square x))))
		(lambda (i) (- (* i 2) 1))
		k))

(iter-a-to-b
 (lambda (k) (tan-cf 1.0 k))
 1
 20)

(load "./base.scm")

(define (sum-i term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (+ a 1) (+ result (term a)))))
	(iter a 0))

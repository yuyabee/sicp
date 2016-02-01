(load "./chap2/q258.scm")

(define (deriv exp var)
	(cond ((number? exp) 0)
				((variable? exp) (if (same-variable? exp var) 1 0))
				(else ((get 'deriv (operator exp)) (operands exp)
																					 var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (deriv-sum exp var)
	(make-sum
		(deriv (addend exp) var)
		(deriv (augend exp) var)))

(define (deriv-product exp var)
	(make-sum
		(make-product (multiplier exp)
									(deriv (multiplicand exp) var))
		(make-product (deriv (multiplier exp))
									(multiplicand exp))))

(define (install-deriv-package)
 (put 'deriv '+ deriv-sum)
 (put 'deriv '* deriv-product))

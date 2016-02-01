(load "./chap3/restriction.scm")

(define (averager a b c)
	(let ((u (make-connector))
				(x (make-connector)))
		(adder a b u)
		(multiplier c x u)
		(constant 2 x)
		'ok))

(define A (make-connector))
(define B (make-connector))
(define Average (make-connector))

(averager A B Average)

(probe 'A A)
(probe 'B B)
(probe 'Average Average)

(set-value! A 99 'user)
(set-value! B 1 'user)

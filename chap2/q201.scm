(load "./chap2/base.scm")

(define (make-rat n d)
	; gcd always return positive number
	; the sign should come before n so the if tests only d to decide which sign
	; to use
	(let ((g ((if (< d 0) - +) (gcd n d))))
		(cons (/ n g) (/ d g))))

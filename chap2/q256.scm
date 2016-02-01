(define (exponentiation? x)
	(and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (make-exponentiation b e)
	(cond ((=number? e 0) 1)
				((=number? e 1) b)
				(else (list '** b e))))

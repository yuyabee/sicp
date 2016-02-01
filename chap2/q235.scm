(load "./chap2/base.scm")

(define (count-leaves t)
	(accumulate + 0
							(map
								(lambda (x) (cond ((null? x) 0)
																	((not (pair? x)) 1)
																	(else (count-leaves x))))
								t)))

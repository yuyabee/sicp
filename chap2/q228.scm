(define (fringe lst)
	(cond ((null? lst) ())
				((not (pair? lst)) (list lst))
				(else (append (fringe (car lst))
											(fringe (cdr lst))))))

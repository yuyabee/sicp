(load "./chap2/set.scm")
	
(define (adjoin-set x set)
	(if (element-of-set? x set)
		set
		(let ((xb (car set)))
			(cond ((> xb x) (cons x set))
						(else (cons xb (adjoin-set x (cdr set))))))))

(define (adjoin-set x set)
	(cond ((null? set) (list x))
				((= x (car set)) set)
				((< x (car set)) (cons x set))
				(else (cons (car set) (adjoin-set x (cdr set))))))

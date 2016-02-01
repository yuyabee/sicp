(define (last-pair x)
	(if (null? (cdr x))
		x
		(last-pair (cdr x))))

(define (make-cycle x)
	(set-cdr! (last-pair x) x)
	x)

(define (contain-cycle? items)
	(define stored '())
	(define (match? x pat)
		(cond ((null? stored) #f)
					((null? x) #f)
					((null? pat) #t)
					((eq? (car x) (car pat))
					 (match? (cdr x) (cdr pat)))
					(else #f)))
	(define (contain? x)
		(cond ((null? x) #f)
					((match? x stored) #t)
					(else
						(begin (if (null? stored)
										 (set! stored (list (car x)))
										 (set! stored (append stored (list (car x)))))
									 (contain? (cdr x))))))
	(contain? items))

; this is from the internet
; it doesn't work for lists like this:
; '(a b c a c)
; it's not cycle but returns #t
(define (cycle? x)
	(define stored '())
	(define (match? x)
		(if (memq x stored)
			#t
			(begin (set! stored (cons x stored)) #f)))
	(define (iter x)
		(if (not (pair? x))
			#f
			(if (match? (car x))
				#t
				(iter (cdr x)))))
	(iter x))

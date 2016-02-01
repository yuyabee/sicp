(load "./chap2/q207.scm")

(define (par1 r1 r2)
	(div-interval (mul-interval r1 r2)
								(add-interval r1 r2)))

(define (par2 r1 r2)
	(let ((one (make-interval 1 1)))
		(div-interval one
									(add-interval (div-interval one r1)
																(div-interval one r2)))))

(par1 (make-interval 3 4) (make-interval 5 6))
(par2 (make-interval 3 4) (make-interval 5 6))

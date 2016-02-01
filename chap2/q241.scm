(load "./chap2/base.scm")

(define (unique-trio n)
	(flat-map (lambda (i)
							(flat-map (lambda (j)
													(map (lambda (k) (list i j k))
															 (enumerate-interval 1 (- j 1))))
												(enumerate-interval 1 (- i 1))))
						(enumerate-interval 1 n)))

(define (sum-of-trio trio)
	(+ (car trio) (cadr trio) (caddr trio)))

(define (equal-sum-of-trio n s)
	(filter (lambda (x) (= (sum-of-trio x) s)) (unique-trio n)))

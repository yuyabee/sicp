(define (cont-frac-r n d k)
	(define (term i)
		(if (= i k)
			(/ (n i) (d i))
			(/ (n i) (+ (d i) (term (+ i 1))))))
	(term 1))

(define (cont-frac-i n d k)
	(define (iter i result)
		(if (= i 0)
			result
			(iter (- i 1) (/ (n i) (+ (d i) result)))))
	(iter k 0.0))

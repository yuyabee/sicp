(define (accumulate-r combiner null-value term a next b)
	(if (> a b) null-value
		(combiner (term a) (accumulate-r combiner null-value term (next a) next b))))

(define (sum term a next b)
	(accumulate-r + 0 term a next b))

(define (accumulate-i combiner null-value term a next b)
 (define (iter a res)
	(if (> a b) res
	 (iter (next a) (combiner res (term a)))))
 (iter a null-value))

(define (product term a next b)
 (accumulate-i * 1 term a next b))

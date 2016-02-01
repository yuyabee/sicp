(load "./prime.scm")

(define (filtered-accumulate combiner null-value term a next b filter)
	(if (> a b) null-value
		(if (filter a)
			(combiner (term a)
								(filtered-accumulate
									combiner null-value term (next a) next b filter))
			(combiner null-value
								(filtered-accumulate
									combiner null-value term (next a) next b filter)))))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (sum-of-prime-squares a b)
	(filtered-accumulate + 0 square a inc b prime?))

(define (product-of-relative-primes n)
 (define (filter x)
	(= 1 (gcd x n)))
 (filtered-accumulate * 1 identity 1 inc n filter))

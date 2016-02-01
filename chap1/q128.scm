(load "./prime.scm")

(define (miller-rabin-test x n)
	(and (not (= x 1))
			 (not (= x (- n 1)))
			 (= (remainder (square x) n) 1)))

(define (expmod base exp m)
	(cond ((= exp 0) 1)
				((miller-rabin-test base m) 1)
				((even? exp)
				 (remainder (square (expmod base (/ exp 2) m))
										m))
				(else
					(remainder (* base (expmod base (- exp 1) m))
										 m))))

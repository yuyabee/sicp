(load "./base.scm")

(define (inc n) (+ n 1))

(define (simpsons-integral f a b n)
	(define h (/ (- b a) n))
	(define (yk k) (f (+ a (* k h))))
	(define (simpsons-term k)
		(* (cond ((or (= k 0) (= k n)) 1)
						 ((odd? k) 4)
						 (else 2))
			 (yk k)))
	(* (/ h 3) (sum simpsons-term 0 inc n)))

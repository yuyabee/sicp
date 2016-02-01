(define (square x) (* x x))

(define (sum-of-squares x y) (+ (square x) (square y)))

(define (big-two x y z) (cond ((and (> 0 (- x y)) (> 0 (- x z)) (values y z)))
															((and (> 0 (- y x)) (> 0 (- y z)) (values x z)))
															(else (values x y))))

(define (q13 x y z) (cond ((and (> 0 (- x y)) (> 0 (- x z)) (sum-of-squares y z)))
													((and (> 0 (- y x)) (> 0 (- y z)) (sum-of-squares x z)))
													(else (sum-of-squares x y))))

(define (sqrt-iter guess x)
 (if (sqrt-good-enough? guess x)
	guess
	(sqrt-iter (sqrt-improve guess x)
	 x)))

(define (sqrt-improve guess x)
 (average guess (/ x guess)))

(define (average x y)
 (/ (+ x y) 2))

(define (sqrt-good-enough? guess x)
 (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
 (sqrt-iter 1.0 x))

(define (sqrt-good-enough?2 guess pre-guess x)
 ; when the difference made by improve is really small,
 ; it ignores the rest(by telling the guess is good enough)
 (< (/ (abs (- guess pre-guess)) guess) 0.001))

(define (sqrt-iter2 guess pre-guess x)
 (if (sqrt-good-enough?2 guess pre-guess x)
	guess
	(sqrt-iter2 (sqrt-improve guess x) guess x)))

(define (sqrt2 x)
 (sqrt-iter2 1.0 100.0 x))

(define (fib n)
	(cond ((= n 0) 0)
				((= n 1) 1)
				(else (+ (fib (- n 1))
								 (fib (- n 2))))))

(define (count-change amount)
 ; this 5 is the initialized value which is the largest value in the coins
	(cc amount 5))

(define (cc amount kinds-of-coins)
	(cond ((= amount 0) 1)
				((or (< amount 0) (= kinds-of-coins 0)) 0)
				(else (+ (cc amount
										 (- kinds-of-coins 1))
								 (cc (- amount
												(first-denomination kinds-of-coins))
										 kinds-of-coins)))
				))

(define (first-denomination kinds-of-coins)
	(cond ((= kinds-of-coins 1) 1)
				((= kinds-of-coins 2) 5)
				((= kinds-of-coins 3) 10)
				((= kinds-of-coins 4) 25)
				((= kinds-of-coins 5) 50)))

(define (expt b n)
 (if (= n 0) 1
	(* b (expt b (- n 1)))))

(define (fast-expt b n)
	(cond ((= n 0) 1)
				((even? n) (square (fast-expt b (/ n 2))))
				(else (* b (fast-expt b (- n 1))))))

(define (even? n)
	(= (remainder n 2) 0))

(define (gcd a b)
	(if (= b 0)
		a
		(gcd b (remainder a b))))

(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a)
			 (sum term (next a) next b))))

(define (integral f a b dx)
	(define (add-dx x) (+ x dx))
	(* (sum f (+ a (/ dx 2.0)) add-dx b)
		 dx))

(define (cube n)
 (* n n n))

(define (identity x) x)

(define (inc n) (+ n 1))

(define (search f neg-point pos-point)
	(let ((mid-point (average neg-point pos-point)))
		(if (close-enough? neg-point pos-point)
			mid-point
			(let ((test-value (f mid-point)))
				(cond ((positive? test-value)
							 (search f neg-point mid-point))
							((negative? test-value)
							 (search f mid-point pos-point))
							(else mid-point))))))

(define (close-enough? x y)
 (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
 (let ((a-value (f a))
			 (b-value (f b)))
	(cond ((and (negative? a-value) (positive? b-value))
				 (search f a b))
	 ((and (negative? b-value) (positive? a-value))
		(search f b a))
	 (else (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance))
	(define (try guess)
		(let ((next (f guess)))
			(if (close-enough? guess next)
				next
				(try next))))
	(try first-guess))

(define (iter-a-to-b f a b)
	(newline)
	(display a)
	(display " -> ")
	(if (> a b)
		(f a)
		(and (display (f a)) (iter-a-to-b f (+ a 1) b))))

(define (average-damp f)
 (lambda (x) (average x (f x))))

(define (sqrt x)
	(fixed-point (average-damp (lambda (y) (/ x y)))
							 1.0))

(define (cube-root x)
	(fixed-point (average-damp (lambda (y) (/ x (square y))))
							 1.0))

(define dx 0.00001)

(define (deriv g)
	(lambda (x)
		(/ (- (g (+ x dx)) (g x))
			 dx)))

(define (newton-transform g)
 (lambda (x)
	(- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
 (fixed-point (newton-transform g) guess))

(define (sqrt x)
	(newtons-method (lambda (y) (- (square y) x))
								 1.0))

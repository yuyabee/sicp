(load "./base")

(define (cbrt-iter guess pre-guess x)
 (if (cbrt-good-enough? guess pre-guess x)
	guess
	(cbrt-iter (cbrt-improve guess x) guess x)))

(define (cbrt-good-enough? guess pre-guess x)
 (< (/ (abs (- guess pre-guess)) guess) 0.001))

(define (cbrt-improve guess x)
 (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cbrt x)
 (cbrt-iter 1.0 100.0 x))

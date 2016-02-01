(load "./chap2/complex_num.scm")

(define (square x) (* x x))

(define (real-part z)
	(* (magnitude z) (cos (angle z))))

(define (imag-part z)
	(* (magnitude z) (sin angle z)))

(define (magnitude z) (car z))

(define (angle z) (cdr z))

(define (make-from-real-imag x y)
	(cons (sqrt (+ (square x) (square y)))
				(atan y x)))

(define (make-from-mag-ang r a) (cons r a))

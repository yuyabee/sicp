(load "./chap2/q256.scm")
(load "./chap2/q257.scm")

(define (deriv exp var)
	(cond ((number? exp) 0)
				((variable? exp)
				 (if (same-variable? exp var) 1 0))
				((sum? exp)
				 (make-sum (deriv (addend exp) var)
									 (deriv (augend exp) var)))
				((product? exp)
				 (make-sum
					 (make-product (multiplier exp)
												 (deriv (multiplicand exp) var))
					 (make-product (deriv (multiplier exp) var)
												 (multiplicand exp))))
				((exponentiation? exp)
				 (make-product
					 (make-product
						 (exponent exp)
						 (make-exponentiation (base exp) (- (exponent exp) 1)))
					 (deriv (base exp) var)))
				(else
					(error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
	(cond ((=number? a1 0) a2)
				((=number? a2 0) a1)
				((and (number? a1) (number? a2)) (+ a1 a2))
				(else (list '+ a1 a2))))

(define (make-product m1 m2)
	(cond ((or (=number? m1 0) (=number? m2 0)) 0)
				((=number? m1 1) m2)
				((=number? m2 1) m1)
				((and (number? m1) (number? m2)) (* m1 m2))
				(else (list '* m1 m2))))

(define (sum? x)
	(and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
	(and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (=number? exp num)
	(and (number? exp) (= exp num)))

(define (apply-generic op . args)
	(let ((type-tags (map type-tag args)))
		(let ((proc (get op type-tags)))
			(if proc
				(apply proc (map contents args))
				(if (= (length args) 2)
					(let ((type1 (car type-tags))
								(type2 (cadr type-tags))
								(a1 (car args))
								(a2 (cadr args)))
						(let ((t1->t2 (get-coercion type1 type2))
									(t2->t1 (get-coercion type2 type1)))
							(cond (t1->t2
											(apply-generic op (t1->t2 a1) a2))
										(t2->t1
											(apply-generic op a1 (t2->t1 a2)))
										(else
											(error "No method for these types"
														 (list op type-tags))))))
					(error "No method for these types" (list op type-tags)))))))

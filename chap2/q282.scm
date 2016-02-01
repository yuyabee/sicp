(define (apply-generic op . args)
	(define (coerce-all args target-type-tag)
		(if (null? args)
			'()
			(let ((proc (get-coercion (type-tag (car args)) target-type-tag)))
				(if proc
					(cons (proc (car args) (coerce-all (cdr args) target-type-tag)))
					(cons (car args) (coerce-all (cdr args) target-type-tag))))))
	(let (type-tags (map type-tag args))
		(define (coercion-all-first-type-tag types)
			(let ((first-type (car types)))
				(if (null? first-type)
					#f
					(let ((first-type-args (coerce-all args first-type)))
						(let ((proc (get op (map type-tag first-type-args))))
							(if proc
								(apply proc (map contents first-type-args))
								(coercion-all-first-type-tag (cdr types))))))))
		(coercion-all-first-type-tag type-tags)))

(load "./chap4/environment.scm")

(define (find-and-do var vars vals func)
	(cond ((null? vars) '())
				((eq? var (car vars)) vals)
				(else
					(find-and-do var (cdr vars) (cdr vals) func))))

(define (scan var vars vals)
	(cond ((null? vars) '())
				((eq? var (car vars)) vals)
				(else (scan var (cdr vars) (cdr vals)))))

(define (find-variable var env)
	(if (eq? env the-empty-environment)
		(error "Unbound variable" var)
		(let ((frame (first-frame env)))
			(cond ((scan var (frame-variables frame) (frame-values frame))
						 => (lambda (vals) (set-car! vals val)))
						(else (find-variable var (enclosing-environment frame)))))))

(define (lookup-variable-value var env)
	(car (find-variable var env)))

(define (set-variable-value! var val env)
	(set-car! (find-variable var env) val))

(define (define-variable! var val env)
	(let ((frame (first-frame env)))
		(cond ((scan var (frame-variables frame) (frame-values frame))
					 => (lambda (vals) (set-car! vals val)))
					(else (add-binding-to-frame! var val frame)))))

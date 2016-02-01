;; original make-procedure
;; (define (make-procedure parameters body env)
;;   (list 'procedure parameters body env))

(define (make-procedure parameters body env)
	;; unmodify ... (x lazy) -> x
	(define (unmodify a)
		(if (symbol? a) a (car a)))
	;; take and save modifier (lazy or lazy-memo).
	(define (modifier a)
		(if (symbol? a) '() (cadr a)))

	(let ((p (map unmodify parameters)) ;;  <- (x y)
				(m (map modifier parameters))) ;; <- (() lazy)
		(list 'procedure p m body env)))

(define (procedure-body p) (cadddr p))
(define (procedure-environment p) (car (cddddr p)))

(define (procedure-modifier p) (caddr p))

(define (apply procedure arguments env)
	(cond ((primitive-procedure? procedure)
				 (apply-primitive-procedure
					 procedure
					 (list-of-arg-values arguments env)))
				((compound-procedure? procedure)
				 (eval-sequence
					 (procedure-body procedure)
					 (extend-environment
						 (procedure-parameters procedure)
						 (list-of-delayed-args
							 arguments
							 (procedure-modifier procedure)
							 env)
						 (procedure-environment procedure))))
				(else
					(error
						"Unknown procedure type -- APPLY" procedure))))

(define (delay-memo-it exp env)
	(list 'thunk-memo exp env))
(define (thunk-memo? obj)
	(tagged-list? obj 'thunk-memo))

(define (list-of-delayed-args exps modifier env) ;; 引数にmodifierが加わった
	(if (no-operands? exps)    '()
		(let ((m (first-operand modifier))
					(o (first-operand exps)))
			(cons (cond ((eq? m 'lazy) (delay-it o env))
									((eq? m 'lazy-memo) (delay-memo-it o env))
									((null? m) (actual-value o env))
									(else (error "Unknown modifier -- LIST-OF-DELAYED-ARGS" m)))
						(list-of-delayed-args (rest-operands exps)
																	env)))))

(define (force-it obj)
	(cond ((thunk-memo? obj)
				 (let ((result (actual-value
												 (thunk-exp obj)
												 (thunk-env obj))))
					 (set-car! obj 'evaluated-thunk)
					 (set-car! (cdr obj) result)
					 (set-cdr! (cdr obj) '())
					 result))
				((evaluated-thunk? obj) (thunk-value obj))
				((thunk? obj) (actual-value (thunk-exp obj) (thunk-env obj)))
				(else obj)))

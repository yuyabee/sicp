;; succeed requires two arguments: evaluated value and
;; fail procedure which will be called when it fails
;; fail doesn't literally fail, but it tries alternative procedure
;; so fail is alternative of the evaluated procedure
;; always set a function that implements a way to come back to the choice to fail
;; argument

(define true #t)
(define false #f)

(define apply-in-underlying-scheme apply)

(define (apply procedure arguments)
	(cond ((primitive-procedure? procedure)
				 (apply-primitive-procedure procedure arguments))
				((compound-procedure? procedure)
				 (eval-sequence
					 (procedure-body procedure)
					 (extend-environment
						 (procedure-parameters procedure)
						 arguments
						 (procedure-environment procedure))))
				(else
					(error
						"Unknown procedure type -- APPLY" procedure))))

(define (eval exp env)
	((analyze exp) env))

;; analyze returns a closure which requires 3 arguments: env, succeed, fail
(define (analyze exp)
	(cond ((self-evaluating? exp)
				 (analyze-self-evaluating exp))
				((quoted? exp) (analyze-quoted exp))
				((variable? exp) (analyze-variable exp))
				((assignment? exp) (analyze-assignment exp))
				((definition? exp) (analyze-definition exp))
				((if? exp) (analyze-if exp))
				((or? exp) (analyze (or->if exp)))
				((and? exp) (analyze (and->if exp)))
				((lambda? exp) (analyze-lambda exp))
				((begin? exp) (analyze-sequence (begin-actions exp)))
				((cond? exp) (analyze (cond->if exp)))
				((let? exp) (analyze (let->combination exp)))
				;; new line
				((amb? exp) (analyze-amb exp))
				((application? exp) (analyze-application exp))
				(else
					(error "Unknown expression type -- ANALYZE" exp))))

(define (or? exp) (tagged-list? exp 'or))

(define (or-clauses exp) (cdr exp))

(define (or-first-exp exp) (car exp))

(define (or-rest-exps exp) (cdr exp))

(define (or->if exp)
	(expand-or-clauses (or-clauses exp)))

(define (expand-or-clauses clauses)
	(if (null? clauses)
		'false
		(let ((first (or-first-exp clauses))
					(rest (or-rest-exps clauses)))
			(make-if first
							 first
							 (expand-or-clauses rest)))))

(define (and? exp) (tagged-list? exp 'and))

(define (and-clauses exp) (cdr exp))

(define (and-first-exp exp) (car exp))

(define (and-rest-exps exp) (cdr exp))

(define (and->if exp)
	(expand-and-clauses (and-clauses exp)))

(define (expand-and-clauses clauses)
	(define (expand-and-iter clauses result)
		(if (null? clauses)
			result
			(let ((first (and-first-exp clauses))
						(rest (and-rest-exps clauses)))
				(make-if first
								 (expand-and-iter rest first)
								 'false))))
	(if (null? clauses)
		'true
		(expand-and-iter clauses '())))

(define (list-of-values exps env)
	(if (no-operands? exps)
		'()
		(cons (eval (first-operand exps) env)
					(list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
	(if (true? (eval (if-predicate exp) env))
		(eval (if-consequent exp) env)
		(eval (if-alternative exp) env)))

(define (eval-sequence exps env)
	(cond ((last-exp? exps) (eval (first-exp exps) env))
				(else (eval (first-exp exps) env)
							(eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
	(set-variable-value! (assignment-variable exp)
											 (eval (assignment-value exp) env)
											 env)
	'ok)

(define (eval-definition exp env)
	(define-variable! (definition-variable exp)
										(eval (definition-value exp) env)
										env)
	'ok)

(define (self-evaluating? exp)
	(cond ((number? exp) true)
				((string? exp) true)
				(else false)))

(define (analyze-self-evaluating exp)
	(lambda (env succeed fail)
		(succeed exp fail)))

(define (variable? exp) (symbol? exp))

(define (analyze-variable exp)
	(lambda (env succeed fail)
		(succeed (lookup-variable-value exp env)
						 fail)))

(define (quoted? exp)
	(tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (analyze-quoted exp)
	(let ((qval (text-of-quotation exp)))
		(lambda (env succeed fail)
			(succeed qval fail))))

(define (tagged-list? exp tag)
	(if (pair? exp)
		(eq? (car exp) tag)
		false))

(define (assignment? exp)
	(tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (analyze-assignment exp)
	(let ((var (assignment-variable exp))
				(vproc (analyze (assignment-value exp))))
		(lambda (env succeed fail)
			(vproc env
						 (lambda (val fail2) ; *1*
							 (let ((old-value
											 (lookup-variable-value var env)))
								 (set-variable-value! var val env)
								 (succeed 'ok
													(lambda () ; *2*
														; need to reset the value of var to go back
														(set-variable-value! var
																								 old-value
																								 env)
														(fail2)))))
						 fail))))

(define (definition? exp)
	(tagged-list? exp 'define))

(define (definition-variable exp)
	(if (symbol? (cadr exp))
		(cadr exp)
		(caadr exp)))

(define (definition-value exp)
	(if (symbol? (cadr exp))
		(caddr exp)
		(make-lambda (cdadr exp)
								 (cddr exp))))

(define (analyze-definition exp)
	(let ((var (definition-variable exp))
				(vproc (analyze (definition-value exp))))
		(lambda (env succeed fail)
			(vproc env
						 (lambda (val fail2)
							 (define-variable! var val env)
							 (succeed 'ok fail2))
						 fail))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
	(cons 'lambda (cons parameters body)))

(define (analyze-lambda exp)
	(let ((vars (lambda-parameters exp))
				(bproc (analyze-sequence (lambda-body exp))))
		(lambda (env succeed fail)
			(succeed (make-procedure vars bproc env)
							 fail))))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
	(if (not (null? (cdddr exp)))
		(cadddr exp)
		'false))

(define (make-if predicate consequent alternative)
	(list 'if predicate consequent alternative))

(define (analyze-if exp)
	(let ((pproc (analyze (if-predicate exp)))
				(cproc (analyze (if-consequent exp)))
				(aproc (analyze (if-alternative exp))))
		(lambda (env succeed fail)
			(pproc env
						 (lambda (pred-value fail2)
							 (if (true? pred-value)
								 (cproc env succeed fail2)
								 (aproc env succeed fail2)))
						 fail))))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
	(cond ((null? seq) seq)
				((last-exp? seq) (first-exp seq))
				(else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (analyze-sequence exps)
	(define (sequentially a b)
		(lambda (env succeed fail)
			(a env
				 (lambda (a-value fail2)
					 (b env succeed fail2))
				 fail)))
	(define (loop first-proc rest-procs)
		(if (null? rest-procs)
			first-proc
			(loop (sequentially first-proc (car rest-procs))
						(cdr rest-procs))))
	(let ((procs (map analyze exps)))
		(if (null? procs)
			(error "Empty sequence -- ANALYZE"))
		(loop (car procs) (cdr procs))))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (analyze-application exp)
	(let ((pproc (analyze (operator exp)))
				(aprocs (map analyze (operands exp))))
		(lambda (env succeed fail)
			(pproc env
						 (lambda (proc fail2)
							 (get-args aprocs
												 env
												 (lambda (args fail3)
													 (execute-application
														 proc args succeed fail3))
												 fail2))
						 fail))))

(define (get-args aprocs env succeed fail)
	(if (null? aprocs)
		(succeed '() fail)
		((car aprocs) env
									(lambda (arg fail2)
										(get-args (cdr aprocs)
															env
															(lambda (args fail3)
																(succeed (cons arg args)
																				 fail3))
															fail2))
									fail)))

(define (execute-application proc args succeed fail)
	(cond ((primitive-procedure? proc)
				 (succeed (apply-primitive-procedure proc args)
									fail))
				((compound-procedure? proc)
				 ((procedure-body proc)
					(extend-environment (procedure-parameters proc)
															args
															(procedure-environment proc))
					succeed
					fail))
				(else
					(error
						"Unknown procedure type -- EXECUTE-APPLICATION"
						proc))))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
	(eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
	(expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
	(if (null? clauses)
		false
		(let ((first (car clauses))
					(rest (cdr clauses)))
			(if (cond-else-clause? first)
				(if (null? rest)
					(sequence->exp (cond-actions first))
					(error "ELSE clause isn't last -- COND->IF"
								 clauses))
				(make-if (cond-predicate first)
								 (let ((action (cond-actions first))
											 (predicate (cond-predicate first)))
									 (if (eq? (car action) '=>)
										 (list (cadr action) predicate)
										 (sequence->exp action)))
								 (expand-clauses rest))))))

(define (let? exp) (tagged-list? exp 'let))

(define (let-clauses exp) (cdr exp))

(define (let-bindings clauses) (car clauses))

(define (let-body clauses) (cdr clauses))

(define (let->combination exp)
	(if (pair? (car (let-clauses exp)))
		(expand-let-clauses (let-clauses exp))
		(expand-named-let-clauses (let-clauses exp))))

(define (expand-let-clauses clauses)
	(if (null? (let-bindings clauses))
		'false
		(cons (make-lambda (map car (let-bindings clauses)) (let-body clauses))
					(map cadr (let-bindings clauses)))))

(define (named-let-var clauses) (car clauses))

(define (named-let-bindings clauses) (cadr clauses))

(define (named-let-body clauses) (caddr clauses))

(define (expand-named-let-clauses clauses)
	(make-begin
		(list
			(list 'define (cons (named-let-var clauses)
													(map car (named-let-bindings clauses)))
						(named-let-body clauses))
			(cons (named-let-var clauses)
						(map cadr (named-let-bindings clauses))))))

(define (amb? exp) (tagged-list? exp 'amb))

(define (amb-choices exp) (cdr exp))

(define (ambeval exp env succeed fail)
	((analyze exp) env succeed fail))

;; what succeed does is passing the result of evaluation to the next step
;; on the other hand, what fail does is going back before the evalution and
;; choose another evalution process
;;
;; (succeed value fail)
;; (fail)
(define (analyze-amb exp)
	(let ((cprocs (map analyze (amb-choices exp))))
		(lambda (env succeed fail)
			(define (try-next choices)
				(if (null? choices)
					(fail)
					((car choices) env
												 succeed
												 (lambda ()
													 (try-next (cdr choices))))))
			(try-next cprocs))))

(define (true? x)
	(not (eq? x false)))

(define (false? x)
	(eq? x false))

(define (scan-out-defines body)
	(define (iter exp vars sets exps)
		(if (null? exp)
			(list (reverse vars) (reverse sets) (reverse exps))
			(if (definition? (car exp))
				(iter (cdr exp)
							(cons (list (definition-variable (car exp))
													''*unassigned*) vars)
							(cons (list 'set! (definition-variable(car exp))
													(definition-value (car exp))) sets) exps)
				(iter (cdr exp) vars sets (cons (car exp) exps)))))
	(define (include-define? exp)
		(if (null? exp)
			false
			(if (definition? (car exp))
				true
				(include-define? (cdr exp)))))
	(if (include-define? body)
		(let ((var-val-exp-list (iter body '() '() '())))
			(list (cons 'let
									(cons (car var-val-exp-list)
												(append (cadr var-val-exp-list)
																(caddr var-val-exp-list))))))
		body))

(define (make-procedure parameters body env)
	(list 'procedure parameters body env))

(define (compound-procedure? p)
	(tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
	(cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
	(set-car! frame (cons var (car frame)))
	(set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
	(if (= (length vars) (length vals))
		(cons (make-frame vars vals) base-env)
		(if (< (length vars) (length vals))
			(error "Too many arguments supplied" vars vals)
			(error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
	(define (env-loop env)
		(define (scan vars vals)
			(cond ((null? vars)
						 (env-loop (enclosing-environment env)))
						((eq? var (car vars))
						 (if (eq? '*unassigned* (car vals))
							 (error "Unassigned variable -- LOOKUP-VARIABLE-VALUE" var)
							 (car vals)))
						(else (scan (cdr vars) (cdr vals)))))
		(if (eq? env the-empty-environment)
			(error "Unbound variable" var)
			(let ((frame (first-frame env)))
				(scan (frame-variables frame)
							(frame-values frame)))))
	(env-loop env))

(define (set-variable-value! var val env)
	(define (env-loop env)
		(define (scan vars vals)
			(cond ((null? vars)
						 (env-loop (enclosing-environment env)))
						((eq? var (car vars))
						 (set-car! vals val))
						(else (scan (cdr vars) (cdr vals)))))
		(if (eq? env the-empty-environment)
			(error "Unbound variable -- SET!" var)
			(let ((frame (first-frame env)))
				(scan (frame-variables frame)
							(frame-values frame)))))
	(env-loop env))

(define (define-variable! var val env)
	(let ((frame (first-frame env)))
		(define (scan vars vals)
			(cond ((null? vars)
						 (add-binding-to-frame! var val frame))
						((eq? var (car vars))
						 (set-car! vals val))
						(else (scan (cdr vars) (cdr vals)))))
		(scan (frame-variables frame)
					(frame-values frame))))

(define primitive-procedures
	(list (list 'car car)
				(list 'cdr cdr)
				(list 'cons cons)
				(list 'null? null?)
				(list 'list list)
				(list 'memq memq)
				(list 'member member)
				(list 'assoc assoc)
				(list 'not not)
				(list '+ +)
				(list '- -)
				(list '* *)
				(list '/ /)
				(list '= =)
				(list '< <)
				(list '> >)
				(list 'abs abs)
				(list 'remainder remainder)
				(list 'print print)
				(list 'true? true?)
				(list 'false? false?)
				(list 'and and)
				(list 'or or)
				(list 'eq? eq?)
				(list 'even? even?)
				(list 'odd? odd?)
				;; add primitive-procedures
				))

(define (primitive-procedure-names)
	(map car
			 primitive-procedures))

(define (primitive-procedure-objects)
	(map (lambda (proc) (list 'primitive (cadr proc)))
			 primitive-procedures))

(define (setup-environment)
	(let ((initial-env
					(extend-environment (primitive-procedure-names)
															(primitive-procedure-objects)
															the-empty-environment)))
		(define-variable! 'true true initial-env)
		(define-variable! 'false false initial-env)
		initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
	(tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
	(apply-in-underlying-scheme
		(primitive-implementation proc) args))


(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
	(define (internal-loop try-again)
		(prompt-for-input input-prompt)
		(let ((input (read)))
			(if (eq? input 'try-again)
				(try-again)
				(begin
					(newline)
					(display ";;; Starting a new problem ")
					(ambeval input
									 the-global-environment
									 (lambda (val next-alternative)
										 (announce-output output-prompt)
										 (user-print val)
										 ;; next-alternative is fail
										 (internal-loop next-alternative))
									 (lambda ()
										 (announce-output
											 ";;; There are no more values of")
										 (user-print input)
										 (driver-loop)))))))
	(internal-loop
		(lambda ()
			(newline)
			(display ";;; There is no current problem")
			(driver-loop))))

(define (prompt-for-input string)
	(newline) (newline) (display string) (newline))

(define (announce-output string)
	(newline) (display string) (newline))

(define (user-print object)
	(if (compound-procedure? object)
		(display (list 'compound-procedure
									 (procedure-parameters object)
									 (procedure-body object)
									 '<procedure-env>))
		(display object)))

; (driver-loop)

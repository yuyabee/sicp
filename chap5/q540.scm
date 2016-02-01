(load "./chap5/compiler.scm")
(load "./chap5/q539.scm")
(load "./chap5/q541.scm")

;; ct-env = compile-time-env
(define (compile exp target linkage ct-env)
	(cond ((self-evaluating? exp)
				 (compile-self-evaluating exp target linkage ct-env))
				((quoted? exp) (compile-quoted exp target linkage ct-env))
				((variable? exp)
				 (compile-variable exp target linkage ct-env))
				((assignment? exp)
				 (compile-assignment exp target linkage ct-env))
				((definition? exp)
				 (compile-definition exp target linkage ct-env))
				((if? exp) (compile-if exp target linkage ct-env))
				((lambda? exp) (compile-lambda exp target linkage ct-env))
				((begin? exp)
				 (compile-sequence (begin-actions exp)
													 target
													 linkage ct-env))
				((cond? exp) (compile (cond->if exp) target linkage ct-env))
				((application? exp)
				 (compile-application exp target linkage ct-env))
				(else
					(error "Unknown expression type -- COMPILE" exp))))

(define (compile-self-evaluating exp target linkage ct-env)
	(end-with-linkage
		linkage
		(make-instruction-sequence '() (list target)
															 `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage ct-env)
	(end-with-linkage
		linkage
		(make-instruction-sequence
			'()
			(list target)
			`((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage ct-env)
	(end-with-linkage
		linkage
		(make-instruction-sequence '(env)
															 (list target)
															 (let ((address (find-variable exp ct-env)))
																 (if (eq? address 'not-found)
																	 `((assign ,target
																						 (op lookup-variable-value)
																						 (const ,exp)
																						 (reg env)))
																	 `((assign ,target
																						 (op lexical-address-lookup)
																						 (const ,address)
																						 (reg env))))))))

(define (compile-assignment exp target linkage ct-env)
	(let ((var (assignment-variable exp))
				(get-value-code
					(compile (assignment-value exp) 'val 'next ct-env)))
		(end-with-linkage
			linkage
			(preserving
				;; val is not saved, because it is modified in get-value-code
				'(env)
				get-value-code
				(make-instruction-sequence '(env val) (list target)
																	 (let ((laddr (find-variable var ct-env)))
																		 (if (eq? laddr 'not-found)
																			 `((perform (op set-variable-value!)
																									(const ,var)
																									(reg val)
																									(reg env))
																				 (assign ,target (const ok)))
																			 `((perform (op lexical-address-set!)
																									(const ,laddr)
																									(reg val)
																									(reg env))
																				 (assign ,target (const ok))))))))))

(define (compile-definition exp target linkage ct-env)
	(let ((var (definition-variable exp))
				(get-value-code
					(compile (definition-value exp) 'val 'next ct-env)))
		(end-with-linkage
			linkage
			(preserving
				;; val is not saved, because it is modified in get-value-code
				'(env)
				get-value-code
				(make-instruction-sequence '(env val) (list target)
																	 `((perform (op define-variable!)
																							(const ,var)
																							(reg val)
																							(reg env))
																		 (assign ,target (const ok))))))))

(define (compile-if exp target linkage ct-env)
	(let ((t-branch (make-label 'true-branch))
				(f-branch (make-label 'false-branch))
				(after-if (make-label 'after-if)))
		(let ((consequent-linkage
						(if (eq? linkage 'next) after-if linkage)))
			(let ((p-code (compile (if-predicate exp) 'val 'next ct-env))
						(c-code
							(compile
								(if-consequent exp) target consequent-linkage ct-env))
						(a-code
							(compile (if-alternative exp) target linkage ct-env)))
				(preserving '(env continue)
										p-code
										(append-instruction-sequences
											(make-instruction-sequence
												'(val)
												'()
												`((test (op false?) (reg val))
													(branch (label ,f-branch))))
											(parallel-instruction-sequences
												(append-instruction-sequences t-branch c-code)
												(append-instruction-sequences f-branch a-code))
											after-if))))))

(define (compile-lambda exp target linkage ct-env)
	(let ((proc-entry (make-label 'entry))
				(after-lambda (make-label 'after-lambda)))
		(let ((lambda-linkage
						(if (eq? linkage 'next) after-lambda linkage)))
			(append-instruction-sequences
				(tack-on-instruction-sequence
					(end-with-linkage
						lambda-linkage
						(make-instruction-sequence '(env) (list target)
																			 `((assign ,target
																								 (op make-compiled-procedure)
																								 (label ,proc-entry)
																								 (reg env)))))
					(compile-lambda-body exp proc-entry ct-env))
				after-lambda))))

(define (compile-lambda-body exp proc-entry ct-env)
	(let ((formals (lambda-parameters exp)))
		(append-instruction-sequences
			(make-instruction-sequence
				'(env proc argl)
				'(env)
				`(,proc-entry
					 (assign env (op compiled-procedure-env) (reg proc))
					 (assign env
									 (op extend-environment)
									 (const ,formals)
									 (reg argl)
									 (reg env))))
			(compile-sequence (lambda-body exp) 'val 'return
												(cons formals ct-env)))))

(define (compile-sequence seq target linkage ct-env)
	(if (last-exp? seq)
		(compile (first-exp seq) target linkage ct-env)
		(preserving '(env continue)
								(compile (first-exp seq) target 'next ct-env)
								(compile-sequence (rest-exps seq) target linkage ct-env))))

(define (compile-application exp target linkage ct-env)
	(let (;; compiling lambda, so it can be called with operands
				(proc-code (compile (operator exp) 'proc 'next ct-env))
				(operand-codes
					(map (lambda (operand) (compile operand 'val 'next ct-env))
							 (operands exp))))
		(preserving
			;; env may be modified in proc-code(when evaluating operands)
			'(env continue)
			proc-code
			(preserving
				;; proc may be modified when constructing operand-codes
				'(proc continue)
				;; making the operands applicable
				;; (could be applied to a function)
				;; by consing them
				(construct-arglist operand-codes)
				(compile-procedure-call target linkage)))))

(define (formatted-compile exp target linkage ct-env)
	(define (output-iter s)
		(if (null? s)
			(newline)
			(begin
				(cond ((pair? (car s))
							 (display "  ")
							 (display (car s))
							 (newline))
							(else
								(display (car s))
								(newline)))
				(output-iter (cdr s)))))
	(let ((compiled-code (compile exp target linkage ct-env)))
		(display ";; needed   : ")
		(display (registers-needed compiled-code))
		(newline)
		(display ";; modified : ")
		(display (registers-modified compiled-code))
		(newline)
		(output-iter (statements compiled-code))))

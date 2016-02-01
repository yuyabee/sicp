(load "./chap5/q540.scm")
	
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
				((open-code? exp ct-env)
				 (compile-open-code exp target linkage ct-env))
				((application? exp)
				 (compile-application exp target linkage ct-env))
				(else
					(error "Unknown expression type -- COMPILE" exp))))

(define (overwrite? exp ct-env)
	(let ((address (find-variable exp ct-env)))
		(eq? address 'not-found)))

(define (open-code? exp ct-env)
	(and (memq (car exp) '(+ - * /))
			 (not (overwrite? exp ct-env))))

(define (spread-arguments args)
	(let ((arg1 (car args))
				(arg2 (cadr args)))
		(list (compile arg1 'arg1 'next)
					(compile arg2 'arg2 'next))))

(define (compile-open-code exp target linkage ct-env)
	(let ((operator (car exp))
				(arguments (spread-arguments (operands exp))))
		(if (> 3 (length (operands exp)))
			(end-with-linkage
				linkage
				(append-instruction-sequences
					(car arguments)
					(preserving
						'(arg1)
						(cadr arguments)
						(make-instruction-sequence
							'(arg1 arg2)
							(list target)
							`((assign ,target (op ,operator) (reg arg1) (reg arg2)))))))
			(begin
				(set-car! (cdr exp) target)
				(set-cdr! (cdr exp) (cdddr exp))
				(end-with-linkage
					linkage
					(append-instruction-sequences
						(car arguments)
						(preserving
							'(arg1)
							(cadr arguments)
							(preserving
								`(,target)
								(make-instruction-sequence
									'(arg1 arg2)
									(list target)
									`((assign ,target (op ,operator) (reg arg1) (reg arg2))))
								(compile-open-code
									exp
									'val 'next)))))))))

(define operands cdr)

(define (identity x) x)

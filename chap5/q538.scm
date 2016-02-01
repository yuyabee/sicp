(load "./chap5/compiler.scm")

(define (compile exp target linkage)
	(cond ((self-evaluating? exp)
				 (compile-self-evaluating exp target linkage))
				((quoted? exp) (compile-quoted exp target linkage))
				((variable? exp)
				 (compile-variable exp target linkage))
				((assignment? exp)
				 (compile-assignment exp target linkage))
				((definition? exp)
				 (compile-definition exp target linkage))
				((if? exp) (compile-if exp target linkage))
				((lambda? exp) (compile-lambda exp target linkage))
				((begin? exp)
				 (compile-sequence (begin-actions exp)
													 target
													 linkage))
				((cond? exp) (compile (cond->if exp) target linkage))
				((open-code? exp) (compile-open-code exp target linkage))
				((application? exp)
				 (compile-application exp target linkage))
				(else
					(error "Unknown expression type -- COMPILE" exp))))

;; b
(define (open-code? exp) (memq (car exp) '(+ - * /)))

;; a
(define (spread-arguments args)
	(let ((arg1 (car args))
				(arg2 (cadr args)))
		(list (compile arg1 'arg1 'next)
					(compile arg2 'arg2 'next))))

(define (compile-open-code exp target linkage)
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

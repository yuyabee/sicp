(load "./chap4/q406.scm")

(define (eval exp env)
	(cond ((self-evaluating? exp) exp)
				((variable? exp) (lookup-variable-value exp env))
				((quoted? exp) (text-of-quotation exp))
				((assignment? exp) (eval-assignment exp env))
				((definition? exp) (eval-definition exp env))
				((if? exp) (eval-if exp env))
				((lambda? exp)
				 (make-procedure (lambda-parameters exp)
												 (lambda-body exp)
												 env))
				((begin? exp) 
				 (eval-sequence (begin-actions exp) env))
				((cond? exp) (eval (cond->if exp) env))
				((let? exp) (eval (let->combination exp) env))
				((letrec? exp) (eval (letrec->let exp) env))
				((application? exp)
				 (apply (eval (operator exp) env)
								(list-of-values (operands exp) env)))
				(else
					(error "Unknown expression type -- EVAL" exp))))

(define (letrec? exp) (tagged-list? exp 'letrec))

(define (letrec-vars exp) (map car (cadr exp)))

(define (letrec-inits exp) (map cadr (cadr exp)))

(define (letrec-body exp) (cddr exp))

(define (letrec->let exp)
	(append (list 'let (map (lambda (sym) (list sym '(quote *unassigned*)))
													(letrec-vars exp)))
					(map (lambda (sym init) (list 'set! sym init))
							 (letrec-vars exp)
							 (letrec-inits exp))
					(letrec-body exp)))

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
				((and? exp) (eval (eval-and exp) env))
				((or? exp) (eval (eval-or exp) env))
				((application? exp)
				 (apply (eval (operator exp) env)
								(list-of-values (operands exp) env)))
				(else
					(error "Unknown expression type -- EVAL" exp))))

(define (and? exp)
	tagged-list? exp 'and)

(define (eval-and exp env)
	(define (eval-conditions exps env)
		(cond ((no-operands? exps) #t)
					((true? (eval (first-operand exps) env))
					 (eval-conditions (rest-operands exps env)))
					(else #f)))
	(eval-conditions (operands exps) env))

(define (or? exp)
	tagged-list? exp 'or)

(define (eval-or exp env)
	(define (eval-conditions exps env)
		(cond ((no-operands? exps) #f)
					((true? (eval (first-operand exps) env)) #t)
					(else (eval-conditions (rest-operands exps) env))))
	(eval-conditions (operands exp) env))

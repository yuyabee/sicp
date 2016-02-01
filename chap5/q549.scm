(load "./chap5/eceval-compiler.scm")

(define (get-machine) compile-assemble-machine)

(define compile-assemble-machine-operations
	(list
		;;primitive Scheme operations
		(list 'read read)			;used by eceval
		(list 'display display)

		;;used by compiled code
		(list 'list list)
		(list 'cons cons)

		;;operations in syntax.scm
		(list 'self-evaluating? self-evaluating?)
		(list 'quoted? quoted?)
		(list 'text-of-quotation text-of-quotation)
		(list 'variable? variable?)
		(list 'assignment? assignment?)
		(list 'assignment-variable assignment-variable)
		(list 'assignment-value assignment-value)
		(list 'definition? definition?)
		(list 'definition-variable definition-variable)
		(list 'definition-value definition-value)
		(list 'lambda? lambda?)
		(list 'lambda-parameters lambda-parameters)
		(list 'lambda-body lambda-body)
		(list 'if? if?)
		(list 'if-predicate if-predicate)
		(list 'if-consequent if-consequent)
		(list 'if-alternative if-alternative)
		(list 'begin? begin?)
		(list 'begin-actions begin-actions)
		(list 'last-exp? last-exp?)
		(list 'first-exp first-exp)
		(list 'rest-exps rest-exps)
		(list 'application? application?)
		(list 'operator operator)
		(list 'operands operands)
		(list 'no-operands? no-operands?)
		(list 'first-operand first-operand)
		(list 'rest-operands rest-operands)

		;;operations in eceval-support.scm
		(list 'true? true?)
		(list 'false? false?)		;for compiled code
		(list 'make-procedure make-procedure)
		(list 'compound-procedure? compound-procedure?)
		(list 'procedure-parameters procedure-parameters)
		(list 'procedure-body procedure-body)
		(list 'procedure-environment procedure-environment)
		(list 'extend-environment extend-environment)
		(list 'lookup-variable-value lookup-variable-value)
		(list 'set-variable-value! set-variable-value!)
		(list 'define-variable! define-variable!)
		(list 'primitive-procedure? primitive-procedure?)
		(list 'apply-primitive-procedure apply-primitive-procedure)
		(list 'prompt-for-input prompt-for-input)
		(list 'announce-output announce-output)
		(list 'user-print user-print)
		(list 'empty-arglist empty-arglist)
		(list 'adjoin-arg adjoin-arg)
		(list 'last-operand? last-operand?)
		(list 'no-more-exps? no-more-exps?)	;for non-tail-recursive machine
		(list 'get-global-environment get-global-environment)

		;;for compiled code (also in eceval-support.scm)
		(list 'make-compiled-procedure make-compiled-procedure)
		(list 'compiled-procedure? compiled-procedure?)
		(list 'compiled-procedure-entry compiled-procedure-entry)
		(list 'compiled-procedure-env compiled-procedure-env)
		;; to run compile-time-env included compiler
		;(list 'lexical-address-lookup lexical-address-lookup)
		;(list 'make-compiled-procedure make-compiled-procedure)
		
		(list 'compile compile)
		(list 'assemble assemble)
		(list 'get-machine get-machine)
		(list 'statements statements)
		))

(set! the-global-environment (setup-environment))

(define compile-assemble-machine
	(make-machine
		'(exp env val proc argl continue unev compapp machine)
		compile-assemble-machine-operations
		'(
			compile-start
			(assign machine (op get-machine))
			read-eval-print-loop
			(perform (op initialize-stack))
			(perform
				(op prompt-for-input) (const ";;; Compile-and-Assemble input:"))
			(assign exp (op read))
			(assign env (op get-global-environment))
			(assign continue (label print-result))
			(goto (label compile))
			print-result
			(perform (op print-stack-statistics))
			(perform
				(op announce-output) (const ";;; Compile-and-Assemble value:"))
			(perform (op user-print) (reg val))
			(goto (label read-eval-print-loop))
			compile
			(assign exp (op compile) (reg exp) (const val) (const return))
			(assign exp (op statements) (reg exp))
			(perform (op display) (reg exp))
			(goto (label assemble))
			assemble
			(assign val (op assemble) (reg exp) (reg machine))
			(goto (reg val))
			)))

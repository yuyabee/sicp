(load "./chap4/analyze_eval.scm")

(define (analyze exp)
	(cond ((self-evaluating? exp) 
				 (analyze-self-evaluating exp))
				((quoted? exp) (analyze-quoted exp))
				((variable? exp) (analyze-variable exp))
				((assignment? exp) (analyze-assignment exp))
				((definition? exp) (analyze-definition exp))
				((if? exp) (analyze-if exp))
				((unless? exp) (analyze (unless->if exp)))
				((lambda? exp) (analyze-lambda exp))
				((begin? exp) (analyze-sequence (begin-actions exp)))
				((cond? exp) (analyze (cond->if exp)))
				((application? exp) (analyze-application exp))
				(else
					(error "Unknown expression type -- ANALYZE" exp))))

(define (unless? exp)
	(tagged-list? exp 'unless))

(define (unless-predicate exp) (cadr exp))

(define (unless-consequent exp) (caddr exp))

(define (unless-alternative exp)
	(if (not (null? (cdddr exp)))
		(cadddr exp)
		false))

(define (unless->if exp)
	(make-if (unless-predicate exp)
					 (unless-alternative exp)
					 (unless-consequent)))

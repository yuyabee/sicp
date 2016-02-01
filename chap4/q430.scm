(load "./chap4/lazy_eval.scm")

(define (eval-sequence exps env)
	(cond ((last-exp? exps) (eval (first-exp exps) env))
				(else (actual-value (first-exp exps) env)
							(eval-sequence (rest-exps exps) env))))

(driver-loop)

(load "./chap4/analyze_eval.scm")

(define (driver-loop)
	(prompt-for-input input-prompt)
	(let ((input (read)))
		(let ((output (eval input the-global-environment)))
			(announce-output output-prompt)
			(user-print output)))
	(driver-loop))

(define (driver-loop)
	(prompt-for-input input-prompt)
	(let ((input (read)))
		(let ((output (time (eval input the-global-environment))))
			(announce-output output-prompt)
			(user-print output)))
	(driver-loop))

(define (fib n)
	(define (fib-iter a b count)
		(if (= count 0)
			b
			(fib-iter (+ a b) a (- count 1))))
	(fib-iter 1 0 n))

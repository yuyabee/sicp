(load "./chap5/q539.scm")

(define (find-variable var env)
	(define (frame-number-iter frames frame-number)
		(if (null? frames)
			'not-found
			(let ((address (displacement-number-iter (car frames) frame-number 0)))
				(if (not (null? address))
					address
					(frame-number-iter (cdr frames) (+ 1 frame-number))))))
	(define (displacement-number-iter frame frame-number displacement-number)
		(if (null? frame)
			'()
			(if (eq? (car frame) var)
				(make-lexical-address frame-number displacement-number)
				(displacement-number-iter (cdr frame) frame-number
																	(+ 1 displacement-number)))))
	(frame-number-iter env 0))

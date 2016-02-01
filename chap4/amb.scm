(define (require p)
	(if (not (true? p)) (amb)))

(define (an-element-of items)
	(require (not (null? items)))
	(amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
	(amd n (an-integer-starting-from (+ n 1))))

(define (make-segment start end)
	(cons start end))

(define (start-segment y)
	(car y))

(define (end-segment y)
	(cdr y))

(define (make-point x y)
	(cons x y))

(define (x-point p)
	(car p))

(define (y-point p)
	(cdr p))

(define (middle-point-segment seg)
	(define (average a b) (/ (+ a b) 2))
	(let ((start (start-segment seg))
				(end (end-segment seg)))
		(make-point (average (x-point end) (x-point start))
								(average (y-point end) (y-point start)))))

(define (print-point p)
 (newline)
 (display "(")
 (display (x-point p))
 (display ",")
 (display (y-point p))
 (display ")"))

(define start-point (make-point 1 2))

(define end-point (make-point 4 8))

(define seg (make-segment start-point end-point))

(print-point (middle-point-segment seg))

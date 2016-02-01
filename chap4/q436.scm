(load "./chap4/amb.scm")
(load "./chap4/q435.scm")

(define (a-pythagorean-triple-from n)
	(let ((k (an-integer-starting-from n)))
		(let ((i (an-integer-between n k)))
			(let ((j (an-integer-between i k)))
				(require (= (+ (* i i) (* j j)) (* k k)))
				(list i j k)))))

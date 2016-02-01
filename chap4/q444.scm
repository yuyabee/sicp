(define (solve)
	(let ((q1 (amb 1 2 3 4 5 6 7 8))
				(q2 (amb 1 2 3 4 5 6 7 8))
				(q3 (amb 1 2 3 4 5 6 7 8))
				(q4 (amb 1 2 3 4 5 6 7 8))
				(q5 (amb 1 2 3 4 5 6 7 8))
				(q6 (amb 1 2 3 4 5 6 7 8))
				(q7 (amb 1 2 3 4 5 6 7 8))
				(q8 (amb 1 2 3 4 5 6 7 8)))
		(require (distinct? (list q1 q2 q3 q4 q5 q6 q7 q8)))
		(require (safe-pos? q1 (list q2 q3 q4 q5 q6 q7 q8)))
		(require (safe-pos? q2 (list q1 q3 q4 q5 q6 q7 q8)))
		(require (safe-pos? q3 (list q1 q2 q4 q5 q6 q7 q8)))
		(require (safe-pos? q4 (list q1 q2 q3 q5 q6 q7 q8)))
		(require (safe-pos? q5 (list q1 q2 q3 q4 q6 q7 q8)))
		(require (safe-pos? q6 (list q1 q2 q3 q4 q5 q7 q8)))
		(require (safe-pos? q7 (list q1 q2 q3 q4 q5 q6 q8)))
		(require (safe-pos? q8 (list q1 q2 q3 q4 q5 q6 q7)))
		(list q1 q2 q3 q4 q5 q6 q7 q8)))

(define (distinct? items)
	(cond ((null? items) true)
				((null? (cdr items)) true)
				((member (car items) (cdr items)) false)
				(else (distinct? (cdr items)))))

(define (safe-pos? q q-list)
	(if (null? q-list)
		true
		(if (= (abs (- q (car q-list))) (length q-list))
			false
			(safe-pos? q (cdr q-list)))))

(define (length lis)
	(define (iter lis n)
		(if (null? lis)
			n
			(iter (cdr lis) (+ n 1))))
	(iter lis 0))

(define (require p)
	(if (not (true? p)) (amb)))

(define (queen-puzzle)
	(let ((q1 (amb 1 2 3 4 5 6 7 8)))
		(let ((q2 (amb 1 2 3 4 5 6 7 8)))
			(require (distinct? (list q1 q2)))
			(require (safe-pos? q2 (list q1)))
			(let ((q3 (amb 1 2 3 4 5 6 7 8)))
				(require (distinct? (list q1 q2 q3)))
				(require (safe-pos? q3 (list q1 q2)))
				(let ((q4 (amb 1 2 3 4 5 6 7 8)))
					(require (distinct? (list q1 q2 q3 q4)))
					(require (safe-pos? q4 (list q1 q2 q3)))
					(let ((q5 (amb 1 2 3 4 5 6 7 8)))
						(require (distinct? (list q1 q2 q3 q4 q5)))
						(require (safe-pos? q5 (list q1 q2 q3 q4)))
						(let ((q6 (amb 1 2 3 4 5 6 7 8)))
							(require (distinct? (list q1 q2 q3 q4 q5 q6)))
							(require (safe-pos? q6 (list q1 q2 q3 q4 q5)))
							(let ((q7 (amb 1 2 3 4 5 6 7 8)))
								(require (distinct? (list q1 q2 q3 q4 q5 q6 q7)))
								(require (safe-pos? q7 (list q1 q2 q3 q4 q5 q6)))
								(let ((q8 (amb 1 2 3 4 5 6 7 8)))
									(require (distinct? (list q1 q2 q3 q4 q5 q6 q7 q8)))
									(require (safe-pos? q8 (list q1 q2 q3 q4 q5 q6 q7)))
									(list q1 q2 q3 q4 q5 q6 q7 q8))))))))))

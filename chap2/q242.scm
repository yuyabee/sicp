(load "./chap2/base.scm")

(define (queens board-size)
	(define (queen-cols k)
		(if (= k 0)
			(list empty-board)
			(filter
				(lambda (positions) (print "positions: " positions) (safe? k positions))
				(flat-map
					(lambda (rest-of-queens)
						(print "rest-of-queens: " rest-of-queens)
						(map (lambda (new-row)
									 (adjoin-position new-row k rest-of-queens))
								 (enumerate-interval 1 board-size)))
					(queen-cols (- k 1))))))
	(queen-cols board-size))

(define empty-board ())

(define (adjoin-position row col rest)
	(cons (list row col) rest))

(define (safe? k positions)
	(let ((kth (car positions)))
		(define (iter rest)
			(print "kth " kth ", rest: " rest)
			(cond ((null? rest) #t)
						((conflicts? (car rest) kth) #f)
						(else (iter (cdr rest)))))
		(iter (cdr positions))))

(define (conflicts? a b)
	(let ((dx (abs (- (car a) (car b))))
				(dy (abs (- (cadr a) (cadr b)))))
		(cond ((= dx 0) #t)
					((= dy 0) #t)
					((= dx dy) #t)
					(else #f))))

(define (count-change amount)
	; this 5 is the initialized value which is the largest value in the coins
	(cc amount 5))

(define (cc amount coin-values)
	(cond ((= amount 0) 1)
				((or (< amount 0) (no-more? coin-values)) 0)
				(else (+ (cc amount
										 (except-first-denomination coin-values))
								 (cc (- amount
												(first-denomination coin-values))
										 coin-values)))
				))

(define (no-more? coin-values)
 (null? (cdr coin-values)))

(define (except-first-denomination coin-values)
 (cdr coin-values))

(define (first-denomination coin-values)
 (car coin-values))

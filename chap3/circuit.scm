(load "./chap3/agenda.scm")

(define (half-adder a b s c)
	(let ((d (make-wire))
				(e (make-wire)))
		(or-gate a b d)
		(and-gate a b c)
		(inverter c e)
		(and-gate d e s))
	'ok)

(define (full-adder a b c-in sum c-out)
	(let ((s (make-wire))
				(c1 (make-wire))
				(c2 (make-wire)))
		(half-adder b c-in s c1)
		(half-adder a s sum c2)
		(or-gate c1 c2 c-out)))

(define (inverter input output)
	(define (invert-input)
		(let ((new-value (logical-not (get-signal input))))
			(after-delay inverter-delay
									 (lambda () (set-signal! output new-value)))))
	(add-action! input invert-input)
	'ok)

(define (logical-not s)
	(cond ((= s 0) 1)
				((= s 1) 0)
				(else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
	(define (and-function-procedure)
		(let ((new-value (logical-and (get-signal a1) (get-signal a2))))
			(after-delay and-gate-delay
									 (lambda () (set-signal! output new-value)))))
	(add-action! a1 and-function-procedure)
	(add-action! a2 and-function-procedure)
	'ok)

(define (logical-and s1 s2)
	(cond ((and (= s1 1) (= s2 1)) 1)
				((and (= s1 1) (= s2 0)) 0)
				((and (= s1 0) (= s2 0)) 0)
				((and (= s1 0) (= s2 1)) 0)
				(else (error "Ivalid signal" s1 s2))))

; or-gate
(load "./chap3/q328")

(define (make-wire)
	(let ((signal-value 0)
				(action-procedures '()))

		(define (set-my-signal! new-value)
			(if (not (= signal-value new-value))
				(begin (set! signal-value new-value)
							 (call-each action-procedures))
				'done))

		(define (accept-action-procedure! proc)
			(set! action-procedures (cons proc action-procedures))
			(proc))

		(define (dispatch m)
			(cond ((eq? m 'get-signal) signal-value)
						((eq? m 'set-signal!) set-my-signal!)
						((eq? m 'add-action!) accept-action-procedure!)
						(else (error "Unknown operation -- WIRE" m))))
		dispatch))

(define (call-each procedures)
	(if (null? procedures)
		'done
		(begin
			((car procedures))
			(call-each (cdr procedures)))))

(define (get-signal wire)
	(wire 'get-signal))

(define (set-signal! wire new-value)
	((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
	((wire 'add-action!) action-procedure))

(define (after-delay delay action)
	(add-to-agenda! (+ delay (current-time the-agenda))
									action
									the-agenda))

(define (propagate)
	(if (empty-agenda? the-agenda)
		'done
		(let ((first-item (first-agenda-item the-agenda)))
			(first-item)
			(remove-first-agenda-item! the-agenda)
			(propagate))))

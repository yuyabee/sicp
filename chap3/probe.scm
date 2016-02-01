(load "./chap3/circuit.scm")

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (probe name wire)
	(add-action! wire
							 (lambda ()
								 (newline)
								 (display name)
								 (display " ")
								 (display (current-time the-agenda))
								 (display " New-value = ")
								 (display (get-signal wire)))))

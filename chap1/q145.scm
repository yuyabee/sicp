(load "./base.scm")
(load "./q143.scm")

(define (n-th-sqrt x n)
  (let ((c (round (/ (log n) (log 2)))))
    (fixed-point ((repeated average-damp c)
                  (lambda (y) (/ x (expt y (- n 1)))))
                 1.0)))

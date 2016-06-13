(use srfi-27)
(load "./chap3/base.scm")

(define (estimate-integral p x1 x2 y1 y2 trials)
  (* (- y2 y1) (- x2 x1)
     (monte-carlo trials
                  (lambda ()
                    (p (random-in-range x1 x2) (random-in-range y1 y2))))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random-integer range))))

(load "./q142.scm")

(define (repeated f n)
  (define (iter i res)
    (if (= i 0)
      res
      (iter (- i 1) (compose f res))))
  (iter n (lambda (x) x)))

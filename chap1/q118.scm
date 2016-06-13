(load "./q116.scm")
(load "./q117.scm")

(define (fast-*-i a b)
  (define (iter base product count)
    (cond ((= count 0) product)
          ((even? count) (iter (double base) product (halve count)))
          (else (iter base (+ product base) (- count 1)))))
  (iter a 0 b))

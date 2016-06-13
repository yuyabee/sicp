(load "./chap2/shape.scm")

(define (split fb fs)
  (define (abs-split painter n)
    (if (= n 0)
      painter
      (let ((smaller (abs-split painter (- n 1))))
        (fb painter (fs smaller smaller)))))
  abs-split)

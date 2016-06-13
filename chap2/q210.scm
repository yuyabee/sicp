(load "./chap2/q207.scm")

(define (div-interval x y)
  (if (> 0 (* (upper-bound y) (lower-bound y)))
    (error "y cannot be 0")
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

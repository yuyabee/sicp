(define (fast-expt-i b n)
  (define (iter b product count)
    (cond ((= count 0) product)
          ((even? count) (iter (square b) product (/ count 2)))
          (else (iter b (* b product) (- count 1)))))
  (iter b 1 n))

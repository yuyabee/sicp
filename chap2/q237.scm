(load "./chap2/q236.scm")

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (transpose mat)
  (accmulate-n cons () mat))

(define (matrix-*-matrix m n)
  (let ((cols (tranpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

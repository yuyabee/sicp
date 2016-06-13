(load "./chap2/q238.scm")

(define (reverse sequence)
  (fold-left
    (lambda (x y) (append (list y) x))
    ()
    sequence))

(define (reverse sequence)
  (fold-right
    (lambda (x y) (append x (list y)))
    ()
    sequence))

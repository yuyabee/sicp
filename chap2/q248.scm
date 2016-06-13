(load "./chap2/base.scm")
(load "./chap2/q246.scm")

(define (make-segment origin-to-start start-to-end)
  (cons origin-to-start start-to-end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

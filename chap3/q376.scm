(load "./chap3/stream.scm")

(define (average a b) (/ (+ a b) 2))

(define (smooth stream)
 (stream-map (lambda (a b) (average a b)) (stream-cdr stream) stream))

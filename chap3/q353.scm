(load "./chap3/stream.scm")

(define s (cons-stream 1 (add-stream s s)))

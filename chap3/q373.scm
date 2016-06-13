(load "./chap3/stream.scm")

(define (RC r c dt)
  (lambda (istream v0)
    (add-stream (scale-stream istream r)
                (integral (scale-stream istream (/ 1 c)) v0 dt))))

(define RC1 (RC 5 1 0.5))

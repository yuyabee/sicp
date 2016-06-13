(define (halve n)
  (if (even? n)
    (/ n 2)
    n))

(define (double n)
  (* n 2))

(define (fast-*-r a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-*-r (double a) (halve b)))
        (else (+ a (fast-*-r a (- b 1))))))

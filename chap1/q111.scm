; recursive is a top-down approch
(define (f-r n)
  (cond ((< n 3) n)
        ; so this is the top of the calculation
        (else (+ (f-r (- n 1)) (* 2 (f-r (- n 2))) (* 3 (f-r (- n 3)))))))

; repetition is a bottom-up approch
(define (f-i n)
  (define (iter a b c count)
    (cond ((= count 0) c)
          ((= count 1) b)
          ((= count 2) a)
          (else (iter b c (+ a (* 2 b) (* 3 c)) (- count 1)))))
  ; the point is that repetition start with the lowest
  ; then climb the calculation
  ; so this is the bottom of the calculation
  (iter 2 1 0 n))

; from the answers
(define (f-i n)
  (define (iter a b c count)
    (cond ((= count 0) c)
          ((= count 1) b)
          (else (iter (+ a (* 2 b) (* 3 c )) a b (- count 1)))))
  (iter 2 1 0 n))

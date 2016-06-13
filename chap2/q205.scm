(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car c)
  (define (count-two n count)
    (if (> (abs (remainder n 2)) 0)
      count
      (count-two (/ n 2) (+ count 1))))
  (count-two c 0))

(define (cdr c)
  (define (count-three n count)
    (if (> (abs (remainder n 3)) 0)
      count
      (count-three (/ n 3) (+ count 1))))
  (count-three c 0))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
; (((add-1 zero) (lambda (x) (+ x 1))) 0)

(define zero (lambda (f) (lambda (x) x)))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define (inc x) (+ x 1))

(define (to-s z)
  ((z inc) 0))

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

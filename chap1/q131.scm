(load "./base.scm")

(define (product term a next b)
  (if (> a b) 1
    (* (term a) (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(define (get-pi n)
  (if (even? n)
    (/ (+ n 2) (+ n 1))
    (/ (+ n 1) (+ n 2))))

(define pi (* (product get-pi 1 inc 100) 4.0))

(define (product-i term a next b)
  (define (iter a result)
    (if (> a b) result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial-i n)
  (product-i identity 1 inc n))

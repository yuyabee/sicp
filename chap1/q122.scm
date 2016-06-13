(load "./prime.scm")

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes from to)
  (define (search-iter current last)
    (if (<= current last) (timed-prime-test current))
    (if (<= current last) (search-iter (+ current 2) last)))
  (search-iter (if (even? from) (+ from 1) from)
               (if (even? to) (- to 1) to)))

(search-for-primes 1000 1019)
(search-for-primes 10000 10037)
(search-for-primes 100000 100043)
(search-for-primes 1000000 1000037)

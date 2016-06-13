(define (deep-reverse l)
  (cond ((not (pair? l)) l)
        (else (reverse (map deep-reverse l)))))

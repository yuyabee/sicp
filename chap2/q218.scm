(define (reverse l)
  (define (iter li res)
    (if (null? li)
      res
      (iter (cdr li) (cons (car li) res))))
  (iter l ()))

(define f
  (let ((n 1))
    (lambda (x)
      (if (= n 0)
        (begin (set! x 0) x)
        x))))

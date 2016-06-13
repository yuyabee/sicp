(define count-pairs
  (let ((stored '()))
    (lambda (x)
      (cond ((not (pair? x)) 0)
            ((memq x stored) 0)
            (else
              (begin (set! stored (cons x stored))
                     (+ (count-pairs (car x))
                        (count-pairs (cdr x))
                        1)))))))

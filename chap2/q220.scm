(define (same-parity x . y)
  (define (iter rest result)
    (if (null? rest)
      result
      (cond ((and (even? x) (even? (car rest)))
             (iter (cdr rest) (append result (list (car rest)))))
            ((and (not (even? x)) (not (even? (car rest))))
             (iter (cdr rest) (append result (list (car rest)))))
            (else
              (iter (cdr rest) result)))))
  (iter y (list x)))

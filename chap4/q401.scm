(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    ;; to evaluate the fist-operand first,
    ;; you need to evaluate fisrt-operand outside of cons
    (let ((value (eval (first-operand exps) env)))
      (cons value (list-of-values (rest-operands exps) env)))))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    ;; to evaluate the rest-operands first,
    ;; you need to apply list-of-values to rest-operands outside of cons
    (let ((value (list-of-values (rest-operands exps) env)))
      (cons (eval (first-operand exps) env)
            value))))

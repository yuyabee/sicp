(load "./chap4/q416.scm")

(define (compile-lambda-body exp proc-entry ct-env)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
      (make-instruction-sequence
        '(env proc argl)
        '(env)
        `(,proc-entry
           (assign env (op compiled-procedure-env) (reg proc))
           (assign env
                   (op extend-environment)
                   (const ,formals)
                   (reg argl)
                   (reg env))))
      (compile-sequence
        (scan-out-defines (lambda-body exp))
        'val
        'return
        (cons formals ct-env)))))

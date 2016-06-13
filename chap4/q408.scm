(load "./chap4/base.scm")

(define (named-let? exp) (symbol? (named-let-name exp)))

(define (named-let-name exp) (cadr exp))

(define (named-let-variables exp) (caddr exp))

(define (named-let-body exp) (cdddr exp))

(define (expand-named-let exp)
  (sequence->exp (list (cons 'define
                             (cons (cons (named-let-name exp)
                                         (let-parameters
                                           (named-let-variables exp)))
                                   (named-let-body exp)))
                       (cons (named-let-name exp)
                             (let-arguments (named-let-variables exp))))))

(define (let->combination exp)
  (if (named-let? exp)
    (expand-named-let exp)
    (cons (make-lambda (let-parameters (let-variables exp))
                       (let-body exp))
          (let-arguments (let-variables exp)))))

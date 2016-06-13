; (do ((x 1 (inc! x)) (y 2 (inc! y))) ((>= x 100) x) (display x)(display y)(newline))
; (let loop ((x 1) (y 2)) (if (>= x 100) x (begin (display x)(display y)(newline)(loop (inc! x) (inc !y)))))

(load "./chap4/base.scm")

(define (do? exp) (tagged-list? exp 'do))

(define (do-bindings exp) (caddr exp))

(define (do-variables bindings)
  (map (lambda (b) (list (car b) (cadr b))) bindings))

(define (do-updates bindings)
  (map (lambda (b) (caddr b)) bindings))

(define (do-predicate exp) (caaddr exp))

(define (do-value exp) (cadr (caddr exp)))

(define (do-body exp) (cdddr exp))

(define (do->named-let exp)
  (list 'let 'loop (do-variables exp)
        (make-if (do-predicate exp)
                 (do-value exp)
                 (sequence->exp (append (do-body exp)
                                        (list (cons 'loop (do-updates (do-bindings exp)))))))))

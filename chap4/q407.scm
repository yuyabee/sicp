(load "./chap4/base.scm")
(load "./chap4/q406.scm")

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (let*? exp) (tagged-list? 'let*))

(define (let*-variables exp) (cadr exp))

(define (let*-body exp) (cddr exp))

(define (let*->nested-lets exp)
  (let ((body (let*-body exp)))
    (define (iterate variables)
      (if (null? variables)
        body
        (list (cons 'let
                    (cons (list (car variables))
                          (iterate (cdr variables)))))))
    (iterate (let*-variables exp))))

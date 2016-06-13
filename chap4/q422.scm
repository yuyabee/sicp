(load "./chap4/analyze_eval.scm")

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp) env))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

(define (let? exp) (tagged-list? exp 'let))

(define (let-variables exp) (cadr exp))

(define (let-body exp) (cddr exp))

(define (let-parameters variables) (map car variables))

(define (let-arguments variables) (map cadr variables))

(define (let->combination exp)
  (cons (make-lambda (let-parameters (let-variables exp))
                     (let-body exp))
        (let-arguments (let-variables exp))))

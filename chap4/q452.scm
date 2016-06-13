(load "./chap4/amb_eval.scm")

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ;; new line
        ((if-fail? exp) (analyze-if-fail exp))
        ((or? exp) (analyze (or->if exp)))
        ((and? exp) (analyze (and->if exp)))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ;; new line
        ((amb? exp) (analyze-amb exp))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

(define (if-fail? exp) (tagged-list? exp 'if-fail))

(define (if-succeed exp) (cadr exp))

(define (if-fail exp) (caddr exp))

(define (analyze-if-fail exp)
  (let ((sproc (analyze (if-succeed exp)))
        (fproc (analyze (if-fail exp))))
    (lambda (env succeed fail)
      (sproc env
             succeed
             (lambda ()
               (fproc env succeed fail))))))

(driver-loop)

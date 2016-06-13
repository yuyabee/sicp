(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((require? exp) (analyze-require exp))
        ((if? exp) (analyze-if exp))
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

(define (require? exp) (tagged-list? exp 'require))

(define (require-predicate exp) (cadr exp))

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not (true? pred-value))
                 ; call it now!!
                 (fail2)
                 (succeed 'ok fail2)))
             fail))))

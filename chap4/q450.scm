(load "./chap4/amb_eval.scm")

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((or? exp) (analyze (or->if exp)))
        ((and? exp) (analyze (and->if exp)))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ;; new line
        ((amb? exp) (analyze-amb exp))
        ((ramb? exp) (analyze-ramb exp))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

(define (ramb? exp) (tagged-list? exp 'amb))

(define (analyze-ramb exp)
  (let ()))

(use srfi-27)

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (define (delete x lst)
      (cond ((null? lst) '())
            ((equal? x (car lst)) (cdr lst))
            (else (cons (car lst) (delete x (cdr lst))))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
          (fail)
          ;; choosing randomly
          (let ((choice (list-ref choices (random-integer (length choices)))))
            (choice env
                    succeed
                    (lambda ()
                      (try-next (delete choice choices)))))))
      (try-next cprocs))))

(driver-loop)

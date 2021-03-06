(load "./chap3/circuit.scm")

; c-out: carry out(繰り上がり)
(define (ripple-carry-adder a-list b-list s-list c-out)
  (define (iter a-list b-list s-list c-in)
    (if (pair? a-list)
      (let ((c-out (make-wire)))
        (full-adder (car a-list) (car b-list) c-in (car s-list) c-out)
        (iter (cdr a-list) (cdr b-list) (cdr s-list) c-out))
      'ok))
  (iter a-list b-list s-list c-out))

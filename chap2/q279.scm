(put 'equ? '(scheme-number scheme-number) (lambda (x y) (= x y)))

(define (equ-rat? x y)
 (and (= (numer x) (numer y)) (= (denom x) (denom y))))

(put 'equ? '(rational rational)
 (lambda (x y) (equ-rat? x y)))

(define (equ-complex? z1 z2)
 (and (= (real-part z1))))) ..... ; easy and boring 
 ;i am not going to complete this

(load "./base.scm")
(load "./q137.scm")

; euler e
(iter-a-to-b (lambda (a)
               (+
                 (cont-frac-i (lambda (i) 1.0)
                              (lambda (i)
                                (if (= (remainder i 3) 2)
                                  (- i (/ (- i 2) 3))
                                  1))
                              a)
                 2))
             1
             20)

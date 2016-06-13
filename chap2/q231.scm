(define (square x) (* x x))

(define (square-tree tree) (tree-map square tree))

(define (tree-map f tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (f tree))
        (else (cons (tree-map f (car tree))
                    (tree-map (cdr tree))))))

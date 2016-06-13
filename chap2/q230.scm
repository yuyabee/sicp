(define (square x) (* x x))

(define (square-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-with-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree-with-map sub-tree)
           (square sub-tree)))
       tree))

(load "./chap2/huffman.scm")

(define (generate-huffman-tree pairs)
	(successive-merge (make-leaf-set pairs)))

; wrong
(define (successive-merge leaf-set)
	(define (iter tree res)
		(if (null? tree)
			res
			(iter (cdr tree) (make-code-tree (car tree) res))))
	(iter (cdr leaf-set) (car leaf-set)))

(define (successive-merge set)
	(if (null? (cdr set))
		(car set)
		(successive-merge
		 ; while accumulating, it is also merging!!
			(adjoin-set (make-code-tree
										(car set)
										(cadr set))
									(cddr set)))))

(define test-tree (generate-huffman-tree '((A 3) (B 5) (C 6) (D 6))))

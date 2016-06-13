(define (parse-word word-list)
  (list (car word-list) (an-element-of (cdr word-list))))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

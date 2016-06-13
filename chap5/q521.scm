(load "./chap5/q517.scm")

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(define count-leaves-machine-a
  (make-machine
    '(continue n val tree tmp val-tmp)
    (list (list 'null? null?)
          (list 'not not)
          (list 'pair? pair?)
          (list '+ +)
          (list 'car car)
          (list 'cdr cdr))
    '(start
       (assign continue (label count-leaves-done))
       (assign val (const 0))
       count-loop
       (test (op null?) (reg tree))
       (branch (label null))
       (assign tmp (op pair?) (reg tree))
       (test (op not) (reg tmp))
       (branch (label not-pair))
       (save continue)
       (assign continue (label count-leaves-on-car))
       (save tree)
       (assign tree (op car) (reg tree))
       (goto (label count-loop))
       null
       (assign val (const 0))
       (goto (reg continue))
       not-pair
       (assign val (const 1))
       (goto (reg continue))
       count-leaves-on-car
       (restore tree)
       (restore continue)
       (assign tree (op cdr) (reg tree))
       (save continue)
       (assign continue (label count-leaves-on-cdr))
       (save val)
       (goto (label count-loop))
       count-leaves-on-cdr
       (assign val-tmp (reg val))
       (restore val)
       (restore continue)
       (assign val (op +) (reg val) (reg val-tmp))
       (goto (reg continue))
       count-leaves-done)))

(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else (count-iter (cdr tree)
                            (count-iter (car tree) n)))))
  (count-iter tree 0))

(define count-leaves-machine-b
  (make-machine
    '(tree n tmp val continue)
    (list (list 'null? null?)
          (list 'not not)
          (list '+ +)
          (list 'pair? pair?)
          (list 'cdr cdr)
          (list 'car car))
    '(start
       (assign continue (label count-leaves-done))
       (assign n (const 0))
       count-iter
       (test (op null?) (reg tree))
       (branch (label null))
       (assign tmp (op pair?) (reg tree))
       (test (op not) (reg tmp))
       (branch (label not-pair))
       (save continue)
       (assign continue (label count-leaves-on-car))
       (save tree)
       (assign tree (op cdr) (reg tree))
       (goto (label count-iter))
       null
       (assign val (reg n))
       (goto (reg continue))
       not-pair
       (assign val (op +) (reg n) (const 1))
       (goto (reg continue))
       count-leaves-on-car
       (restore tree)
       (restore continue)
       (assign tree (op car) (reg tree))
       (assign n (reg val))
       (goto (label count-iter))
       count-leaves-done)))

(count-leaves-machine-a 'trace-on)
(define tree (cons (list 1 2) (list 3 4)))
(set-register-contents! count-leaves-machine-a 'tree (list tree tree))
(start count-leaves-machine-a)

(count-leaves-machine-b 'trace-on)
(set-register-contents! count-leaves-machine-b 'tree (list tree tree))
(start count-leaves-machine-b)

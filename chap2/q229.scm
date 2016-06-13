(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (branch-weight branch)
  (let ((st (branch-structure branch)))
    (cond ((null? branch) 0)
          ((not (pair? st)) st)
          (else (total-weight st)))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (torque branch)
  (let ((st (branch-structure branch)))
    (if (pair? st)
      (+ (torque (left-branch st)) (torque (right-branch st)))
      ; when the structure is not a pair, that is the leaf,
      ; so time to calculate
      (* (branch-length branch) (branch-weight branch)))))

(define (balanced? mobile)
  (= (torque (left-branch mobile)) (torque (right-branch mobile))))

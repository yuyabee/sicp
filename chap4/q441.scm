(load "./chap2/base.scm")

(define (conditions lst)
  (let ((baker (car lst))
        (cooper (cadr lst))
        (fletcher (caddr lst))
        (miller (cadddr lst))
        (smith (car (cddddr lst))))
    (and (not (= baker 5))
         (not (= cooper 1))
         (not (= fletcher 1))
         (not (= fletcher 5))
         (> miller cooper)
         (not (= (abs (- smith fletcher)) 1))
         (not (= (abs (- fletcher cooper)) 1))
         (distinct? lst))))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (permutations s)
  (if (null? s)
    '(())
    (begin
      (flatmap (lambda (x)
                 ;; consing x and a sequence p without x
                 ;; p will be decided before x
                 ;; right to left
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               ;; the function given to flatmap has to make a list of lists
               ;; with s, so that flatmap can append them
               s))))

(define (multiple-dwelling)
  (filter conditions (permutations '(1 2 3 4 5))))

(multiple-dwelling)

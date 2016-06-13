(load "./chap2/q202.scm")

(define (make-rectangle-two-point start end)
  (cons start end))

(define (width rec)
  (let ((start (start-segment rec))
        (end (end-segment rec)))
    (abs (- (x-point start) (x-point end)))))

(define (height rec)
  (let ((start (start-segment rec))
        (end (end-segment rec)))
    (abs (- (y-point start) (y-point end)))))

(define (perimeter-rectangle rec)
  (let ((w (width rec))
        (h (height rec)))
    (* 2 (+ w h))))

(define (area-rectangle rec)
  (* (height rec) (width rec)))

(define (make-rectangle-height-width h w)
  (cons h w))

(define (width rec)
  (cdr rec))

(define (height rec)
  (car rec))

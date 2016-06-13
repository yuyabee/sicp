(load "./chap2/q248.scm")

(define draw-frame-outline
  (let ((v0 (make-vect 0.0 0.0))
        (v1 (make-vect 1.0 0.0))
        (v2 (make-vect 1.0 1.0))
        (v3 (make-vect 0.0 1.0)))
    (segments->painter
      (list (make-segment v0 v1)
            (make-segment v1 v2)
            (make-segment v0 v3)
            (make-segment v3 v2)))))

(define draw-frame-x
  (let ((v0 (make-vect 0.0 0.0))
        (v1 (make-vect 1.0 0.0))
        (v2 (make-vect 1.0 1.0))
        (v3 (make-vect 0.0 1.0)))
    (segments->painter
      (list (make-segment v0 v2)
            (make-segment v1 v3)))))

(define draw-frame-diamond
  (let ((v0 (make-vect 0.5 0.0))
        (v1 (make-vect 1.0 0.5))
        (v2 (make-vect 0.5 1.0))
        (v3 (make-vect 0.0 0.5)))
    (segments->painter
      (list (make-segment v0 v1)
            (make-segment v1 v2)
            (make-segment v2 v3)
            (make-segment v3 v0)))))

; it's wasting of time so skiped
(define draw-frame-wave
  ())

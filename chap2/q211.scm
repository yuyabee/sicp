(define (mul-interval x y)
  (let ((lbx (lower-bound x))
        (ubx (upper-bound x))
        (lby (lower-bound y))
        (uby (upper-bound y)))
    (cond ((> lbx 0)
           (cond ((> lby 0)
                  (make-interval (* lbx lby)
                                 (* ubx uby)))
                 ((< uby 0)
                  (make-interval (* ubx lby)
                                 (* lbx uby)))
                 (else
                   (make-interval (* ubx lby)
                                  (* ubx uby)))))
          ((< ubx 0)
           (cond ((> lby 0)
                  (make-interval (* lbx uby)
                                 (* ubx lby)))
                 ((< uby 0)
                  (make-interval (* ubx uby)
                                 (* lbx lby)))
                 (else
                   (make-interval (* lbx uby)
                                  (* lbx lby)))))
          (else
            (cond ((> lby 0)
                   (make-interval (* lbx uby)
                                  (* ubx uby)))
                  ((< uby 0)
                   (make-interval (* ubx lby)
                                  (* lbx lby)))
                  (else
                    (make-interval (min (* lbx uby) (* ubx lby))
                                   (max (* lbx lby) (* ubx uby)))))))))


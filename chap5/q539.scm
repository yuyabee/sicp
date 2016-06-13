(define (make-lexical-address frame-number displacement-number)
  (list frame-number displacement-number))
(define (lexical-address-frame-number lexical-address)
  (car lexical-address))
(define (lexical-address-displacement-number lexical-address)
  (cadr lexical-address))

(define (make-frame variable value)
  (list variable value))
(define (frame-variable frame) (car frame))
(define (frame-value frame) (cadr frame))
(define (frame-set-value! frame value) (set-car! (cdr frame) value))

(define (lexical-address-get-frame lexical-address env)
  (let ((frame-number (lexical-address-frame-number lexical-address))
        (displacement-number
          (lexical-address-displacement-number lexical-address)))
    (let ((frames (list-ref env frame-number)))
      (let ((frame (list-ref frames displacement-number)))
        (if frame
          frame
          (error "THE FRAME DOES NOT EXIST: " lexical-address))))))

(define (lexical-address-lookup lexical-address env)
  (let ((frame (lexical-address-get-frame lexical-address env)))
    (let ((value (frame-value frame)))
      (if (eq? value '*unassigned*)
        (error "Unassigned variable: " lexical-address)
        value))))

(define (lexical-address-set! lexical-address env value)
  (let ((frame (lexical-address-get-frame lexical-address env)))
    (frame-set-value! frame value)))

(define env '(((x 1) (y 2)) ((a 4) (b 5) (c 7))))

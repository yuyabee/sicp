(load "./chap5/compiler.scm")

(define (preserving regs seq1 seq2)
  (if (null? regs)
    (append-instruction-sequences seq1 seq2)
    (let ((first-reg (car regs)))
      ;; if seq1 modifies register of what seq2 needs, seq1 must be
      ;; surrounded by save and restore
      (preserving (cdr regs)
                  (make-instruction-sequence
                    (list-union (list first-reg)
                                (registers-needed seq1))
                    (list-difference (registers-modified seq1)
                                     (list first-reg))
                    (append `((save ,first-reg))
                            (statements seq1)
                            `((restore ,first-reg))))
                  seq2))))

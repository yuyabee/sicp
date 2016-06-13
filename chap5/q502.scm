(controller
  initialize
  (assign product 1)
  (assign counter 1)
  iter
  (test (op >) (reg counter) (reg n))
  (branch (label factorial-done))
  (assign product (op *) (reg product) (reg counter))
  (assign counter (op +) (reg counter) (const 1))
  (goto (label iter))
  factorial-done)

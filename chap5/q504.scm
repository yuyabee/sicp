(controller
  initialize
  (assign b (op read))
  (assign n (op read))
  (assign continue (label expt-done))
  expt-loop
  (test (op =) (reg n) (const 0))
  (branch (label base-case))
  (save n)
  (save continue)
  (assign n (op -) (reg n) (const 1))
  (assign continue (label after-expt))
  (goto (label expt-loop))
  after-expt
  (restore n)
  (restore continue)
  (assign val (op *) (reg b) (reg val))
  (goto (reg continue))
  base-case
  (assign val (const 1))
  (goto (reg continue))
  expt-done)

(controller
  initialize
  (assign b (op read))
  (assign counter (op read))
  (assign product (const 1))
  iter
  (test (op =) (reg n) (const 0))
  (branch (label expt-done))
  (assign counter (op -) (reg counter) (const 1))
  (assign product (op *) (reg product) (reg b))
  (goto (label iter))
  expt-done)

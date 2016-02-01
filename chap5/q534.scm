gosh> (formatted-compile
		'(define (factorial n)
			(define (iter product counter)
			 (if (> counter n)
				product
				(iter (* counter product) (+ counter 1))))
			(iter 1 1))
		'val 'next)
;; needed   : (env)
;; modified : (val)
  (assign val (op make-compiled-procedure) (label entry35) (reg env))
  (goto (label after-lambda36))
entry35
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (assign val (op make-compiled-procedure) (label entry37) (reg env))
  (goto (label after-lambda38))
entry37
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const >) (reg env))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch42))
compiled-branch43
  (assign continue (label after-call44))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch42
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call44
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch40))
true-branch39
  (assign val (op lookup-variable-value) (const product) (reg env))
  (goto (reg continue))
false-branch40
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch48))
compiled-branch49
  (assign continue (label after-call50))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch48
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call50
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (assign val (op lookup-variable-value) (const product) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch45))
compiled-branch46
  (assign continue (label after-call47))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch45
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call47
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
	;; not saving any value to stack
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch51))
compiled-branch52
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch51
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call53
after-if41
after-lambda38
  (perform (op define-variable!) (const iter) (reg val) (reg env))
  (assign val (const ok))
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch54))
compiled-branch55
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch54
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call56
after-lambda36
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))

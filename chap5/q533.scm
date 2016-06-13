gosh> (formatted-compile
        '(define (factorial n)
           (if (= n 1) 1 (* (factorial (- n 1)) n))) 'val 'next)
;; needed   : (env)
;; modified : (val)
(assign val (op make-compiled-procedure) (label entry1) (reg env))
(goto (label after-lambda2))
entry1
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (n)) (reg argl) (reg env))
(save continue)
(save env)
(assign proc (op lookup-variable-value) (const =) (reg env))
(assign val (const 1))
(assign argl (op list) (reg val))
(assign val (op lookup-variable-value) (const n) (reg env))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch6))
compiled-branch7
(assign continue (label after-call8))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch6
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call8
(restore env)
(restore continue)
(test (op false?) (reg val))
(branch (label false-branch4))
true-branch3
(assign val (const 1))
(goto (reg continue))
false-branch4
(assign proc (op lookup-variable-value) (const *) (reg env))
(save continue)
(save proc)
(assign val (op lookup-variable-value) (const n) (reg env))
(assign argl (op list) (reg val))
(save argl)
(assign proc (op lookup-variable-value) (const factorial) (reg env))
(save proc)
(assign proc (op lookup-variable-value) (const -) (reg env))
(assign val (const 1))
(assign argl (op list) (reg val))
(assign val (op lookup-variable-value) (const n) (reg env))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch9))
compiled-branch10
(assign continue (label after-call11))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch9
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call11
(assign argl (op list) (reg val))
(restore proc)
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch12))
compiled-branch13
(assign continue (label after-call14))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch12
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call14
(restore argl)
(assign argl (op cons) (reg val) (reg argl))
(restore proc)
(restore continue)
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch15))
compiled-branch16
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch15
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(goto (reg continue))
after-call17
after-if5
after-lambda2
(perform (op define-variable!) (const factorial) (reg val) (reg env))
(assign val (const ok))

gosh> (formatted-compile
        '(define (factorial-alt n)
           (if (= n 1) 1 (* n (factorial-alt (- n 1))))) 'val 'next)
;; needed   : (env)
;; modified : (val)
(assign val (op make-compiled-procedure) (label entry18) (reg env))
(goto (label after-lambda19))
entry18
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (n)) (reg argl) (reg env))
(save continue)
(save env)
(assign proc (op lookup-variable-value) (const =) (reg env))
(assign val (const 1))
(assign argl (op list) (reg val))
(assign val (op lookup-variable-value) (const n) (reg env))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch23))
compiled-branch24
(assign continue (label after-call25))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch23
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call25
(restore env)
(restore continue)
(test (op false?) (reg val))
(branch (label false-branch21))
true-branch20
(assign val (const 1))
(goto (reg continue))
false-branch21
(assign proc (op lookup-variable-value) (const *) (reg env))
(save continue)
(save proc)
(save env)
(assign proc (op lookup-variable-value) (const factorial-alt) (reg env))
(save proc)
(assign proc (op lookup-variable-value) (const -) (reg env))
(assign val (const 1))
(assign argl (op list) (reg val))
(assign val (op lookup-variable-value) (const n) (reg env))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch26))
compiled-branch27
(assign continue (label after-call28))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch26
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call28
(assign argl (op list) (reg val))
(restore proc)
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch29))
compiled-branch30
(assign continue (label after-call31))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch29
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call31
(assign argl (op list) (reg val))
(restore env)
(assign val (op lookup-variable-value) (const n) (reg env))
(assign argl (op cons) (reg val) (reg argl))
(restore proc)
(restore continue)
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch32))
compiled-branch33
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch32
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(goto (reg continue))
after-call34
after-if22
after-lambda19
(perform (op define-variable!) (const factorial-alt) (reg val) (reg env))
(assign val (const ok))

#!/usr/bin/env gosh
;; -*- coding: utf-8 -*-
;; file created in 2009/12/28 14:33:16.
;; LastUpdated :2009/12/29 12:02:58.
;; author iNo
;;
;; usage :
;;
;;(define (main args)
;;  0)
;;
;; 問題5.50
;;

(load "./ece4compiler.scm")

(compile-and-go
  '(begin
;;;; 超循環評価器

;; 基盤の apply への参照を apply-in-underlying-scheme へ退避させる（こうすることで、基盤の apply に apply-in-underlying-scheme という名前でアクセスできる）。
(define apply-in-underlying-scheme apply)

;;;; apply の定義
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type -- APPLY" procedure))))

;;;; eval の定義
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval-and (and-clauses exp) env))
        ((or? exp) (eval-or (or-clauses exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((do? exp) (eval (do->let exp) env))
        ((while? exp) (eval (while->let exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

;;;; 手続きの引数
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;;;; 条件式
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;;;; 並び
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;;;; 代入と定義
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;;;; 4.1.2 式の表現

;;;; 自己評価式は数と文字だけ
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;;;; 変数は記号で表現
(define (variable? exp) (symbol? exp))

;;;; クォート式は (quote <text-of-quotation>) の形
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;;;; 代入は (set! <var> <value>) の形
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;;;; 定義
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)      ; 仮パラメタ
                   (cddr exp))))    ; 本体

;;;; lambda 式は記号 lambda で始まるリスト
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;;; 条件式
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;;;; begin
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;;;; 手続き作用
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;;;; cond 式
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      false
      (let ((first (car clauses))
            (rest (cdr clauses)))
           (if (cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last -- COND->IF"
                          clauses))
               (make-if (cond-predicate first)
                        (let ((action (cond-actions first))
                              (predicate (cond-predicate first)))
                             (if (eq? (car action) '=>)
                                 (list (cadr action) predicate)
                                 (sequence->exp action)))
                        (expand-clauses rest))))))

;;;; and
(define (and? exp) (tagged-list? exp 'and))

(define (and-clauses exp) (cdr exp))

(define (and-first-exp exp) (car exp))

(define (and-rest-exps exp) (cdr exp))

(define (eval-and exp env)
  (define (eval-and-iter exp result)
    (if (null? exp)
        result
        (let ((first-eval (eval (and-first-exp exp) env))
              (rest (and-rest-exps exp)))
             (if (true? first-eval)
                 (eval-and-iter rest first-eval)
                 'false))))
  (if (null? exp)
      'true
      (eval-and-iter exp '())))

;;;; or
(define (or? exp) (tagged-list? exp 'or))

(define (or-clauses exp) (cdr exp))

(define (or-first-exp exp) (car exp))

(define (or-rest-exps exp) (cdr exp))

(define (eval-or exp env)
  (if (null? exp)
      'false
      (let ((first-eval (eval (or-first-exp exp) env))
            (rest (or-rest-exps exp)))
           (if (true? first-eval)
               first-eval
               (eval-or rest env)))))

;;;; let 式
(define (let? exp) (tagged-list? exp 'let))

(define (let-clauses exp) (cdr exp))

(define (let-bindings clauses) (car clauses))

(define (let-body clauses) (cdr clauses))

(define (let->combination exp)
  (if (pair? (car (let-clauses exp)))
      (expand-let-clauses (let-clauses exp))
      (expand-named-let-clauses (let-clauses exp))))

(define (expand-let-clauses clauses)
  (if (null? (let-bindings clauses))
      '()
      (cons (make-lambda (map car (let-bindings clauses)) (let-body clauses))
            (map cadr (let-bindings clauses)))))

;;;; 名前付きlet
(define (named-let-var clauses) (car clauses))

(define (named-let-bindings clauses) (cadr clauses))

(define (named-let-body clauses) (caddr clauses))

(define (expand-named-let-clauses clauses)
  (make-begin
    (list
      (list 'define (cons (named-let-var clauses)
                          (map car (named-let-bindings clauses)))
            (named-let-body clauses))
      (cons (named-let-var clauses)
            (map cadr (named-let-bindings clauses))))))

;;;; let*
(define (let*? exp) (tagged-list? exp 'let*))

(define (let*-clauses exp) (cdr exp))

(define (let*-bindings clauses) (car clauses))

(define (let*-body clauses) (cadr clauses))

(define (make-let* defs body)
  (list 'let defs body))

(define (let*->nested-lets exp)
  (if (null? exp)
      'false
      (let ((clauses (let*-clauses exp)))
           (let ((bindings (let*-bindings clauses))
                 (body (let*-body clauses)))
                (define (iter rest-bindings)
                  (if (null? rest-bindings)
                      body
                      (make-let* (list (car rest-bindings))
                                (iter (cdr rest-bindings)))))
           (iter bindings)))))

;;;; do
(define (do? exp) (tagged-list? exp 'do))

(define (do-param-vars exp)
  (define (do-param-vars-aux exp)
    (if (null? exp)
        '()
        (cons (caar exp) (do-param-vars-aux (cdr exp)))))
  (do-param-vars-aux (cadr exp)))

(define (do-param-inits exp)
  (define (do-param-inits-aux exp)
    (if (null? exp)
        '()
        (cons (cadr (car exp)) (do-param-inits-aux (cdr exp)))))
  (do-param-inits-aux (cadr exp)))

(define (do-param-bodies exp)
  (define (do-param-bodies-aux exp)
    (if (or (null? exp) (null? (cddr (car exp))))
        '()
        (cons (caddr (car exp)) (do-param-bodies-aux (cdr exp)))))
  (do-param-bodies-aux (cadr exp)))

(define (do-stop-condition exp) (car (caddr exp)))

(define (do-stop-process exp) (cdr (caddr exp)))

(define (do-body exp) (cdddr exp))

(define (do->let exp)
  (let ((param-vars (do-param-vars exp))
        (param-inits (do-param-inits exp))
        (param-bodies (do-param-bodies exp))
        (stop-condition (do-stop-condition exp))
        (stop-process (do-stop-process exp))
        (body (do-body exp)))
    (list 'let 'do-loop (map list param-vars param-inits)
          (make-if stop-condition
                   (cons 'begin stop-process)
                   (append (cons 'begin  body)
                           (list (cons 'do-loop param-bodies)))))))

;;;; while
(define (while? exp) (tagged-list? exp 'while))

(define (while-predicate exp) (cadr exp))

(define (while-body exp) (cddr exp))

(define (while->let exp)
  (let ((predicate (while-predicate exp))
        (body (while-body exp)))
       (list 'let 'while-loop '()
             (make-if predicate
                      (append (cons 'begin body)
                              (list (cons 'while-loop '())))
                      'true))))

;;;; 4.1.3 評価器のデータ構造

;;;; 述語のテスト
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;;;; 手続きの表現
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

;;;; 環境に対する操作
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
             (scan (frame-variables frame)
                   (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
             (scan (frame-variables frame)
                   (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
       (define (scan vars vals)
         (cond ((null? vars)
                (add-binding-to-frame! var val frame))
               ((eq? var (car vars))
                (set-car! vals val))
               (else (scan (cdr vars) (cdr vals)))))
       (scan (frame-variables frame)
             (frame-values frame))))

;;;; 4.1.4 評価器をプログラムとして走らせる
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        ;; 基本手続きが続く
        (list '+ +)
        (list '* *)
        (list '/ /)
        (list '- -)
        (list '= =)
        (list '> >)
        (list '< <)
        (list 'exit exit)
        ;; ex5.50
        (list 'apply apply)
        (list 'list list)
        (list 'let let)
        (list 'pair? pair?)
        (list '>= >=)
        (list '<= <=)
        (list 'length length)
        (list 'eq? eq?)
        (list 'set-car! set-car!)
        (list 'set-cdr! set-cdr!)
        (list 'newline newline)
        (list 'display display)
        (list 'read read)
        (list 'number? number?)
        (list 'string? string?)
        (list 'symbol? symbol?)
        (list 'cadr cadr)
        (list 'cddr cddr)
        (list 'cdadr cdadr)
        (list 'caadr caadr)
        (list 'cadddr cadddr)
        (list 'caddr caddr)
        (list 'cdddr cdddr)
        (list 'cadadr cadadr)
        (list 'not not)
        (list 'error error)
        ))

(define (map proc lis)
  (if (null? lis)
      '()
      (cons (proc (car lis))
            (map proc (cdr lis)))))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
       (define-variable! 'true true initial-env)
       (define-variable! 'false false initial-env)
       initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))


;;;; 基盤の Lisp システムの"読み込み-評価-印字"ループをモデル化する"駆動ループ(driver loop)"を用意する。
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
       (let ((output (eval input the-global-environment)))
            (announce-output output-prompt)
            (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
;;;;
     ))
;; vim:ft=scheme:fdl=0 fdm=marker:ts=4 sw=4 sts=0:

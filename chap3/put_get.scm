(load "./chap3/table.scm")

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((sub-table (assoc key-1 (cdr local-table))))
        (if sub-table
          (let ((record (assoc key-2 sub-table)))
            (if record
              (cdr record)
              #f))
          #f)))
    (define (insert! key-1 key-2 value)
      (let ((sub-table (assoc key-1 (cdr local-table))))
        (if sub-table
          (let ((record (assoc key-2 (cdr sub-table))))
            (if record
              (set-cdr! record value)
              (set-cdr! sub-table
                        (cons (cons key-2 value)
                              (cdr sub-table)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)
    (lambda (m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else (error "Unknown operation -- TABLE" m))))))

(define operation-table (make-table))

(define get (operation-table 'lookup-proc))

(define put (operation-table 'insert-proc))

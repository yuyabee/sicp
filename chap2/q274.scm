(define (location record)
 (car record)

; you can get the function that works for the specific location of the company
; by giving the abstract function's name and the specified location
(define (get-record record name)
 ((get 'get-record (location record)) record name))

(define (get-salary  record)
 ((get 'get-salary (location record)) record))

(define (find-employee-record records name)
	(if (null? records)
		#f
		(let ((info (get-record (car records) name)))
			(if info
				info
				(find-employee-record (cdr records) name)))))



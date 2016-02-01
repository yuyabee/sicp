(and (supervisor ?name (Ben Bitdiddle))
		 (address ?name . ?where))

(and (salary (Ben Bitdiddle) ?ben_salary)
		 (salary ?person ?amount)
		 (lisp-value < ?amount ?ben_salary))

(and (not (job ?supervisor (computer . ?type)))
		 (supervisor ?person ?supervisor))

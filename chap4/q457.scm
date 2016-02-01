(rule (replace ?person-1 ?person-2)
			(and
				(or (and (job ?person-1 ?job)
								 (job ?person-2 ?job))
						(and (job ?person-1 ?job-1)
								 (job ?person-2 ?job-2)
								 (can-do-job ?job-2 ?job-1)))
				(not (same ?person-1 ?person-2))))

(replace (Fect Cy D) ?who)

(and (salary ?person ?salary)
		 (salary ?substitution ?salary-sub)
		 (replace ?person ?substitution)
		 (lisp-value < ?salary ?salary-sub))

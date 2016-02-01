(rule (last-pair (?x . ()) (?x)))

(rule (last-pair (?x . ?y) ?z)
			(last-pair (?y) ?z))

(rule (grandson-ended ?x)
			(append-to-form ?a (grandson) ?x))

(rule ((grandson) ?a)
			(grandson ?a))

(rule ((great . ?X) ?a ?c)
			(and (son ?b ?c)
					 (?X ?a ?b)
					 (grandson-ended ?X)))

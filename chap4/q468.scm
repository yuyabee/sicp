(assert! (rule (append-to-form () ?y ?y)))

(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
               (append-to-form ?v ?y ?z)))

(assert! (rule (reverse (?x) (?x))))

(assert! (rule (reverse (?u . ?v) ?y)
               (and (reverse ?v ?x)
                    (append-to-form ?x (?u) ?y))))

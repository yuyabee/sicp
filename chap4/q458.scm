(rule (big-shot ?person)
      (and (job ?person (?section . ?type))
           (supervisor ?person ?supervisor)
           (job ?supervisor (?section-sp . ?type-sp))
           (not (same ?section ?section-sp))))

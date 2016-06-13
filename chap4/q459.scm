(meeting ?meeting (Friday ?time))

(rule (meeting-time ?person ?day-and-time)
      (or (meeting whole-company ?day-and-time)
          (and (job ?person (?section . ?type))
               (meeting ?section ?day-and-time))))

(meeting-time (Hacker Alyssa) (Wednessday ?time))

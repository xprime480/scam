(import (test narc))

(narc-label "Match Var True")

(define match-data (match '(:X) '(a)))

(narc-expect
 (#t       (dict? match-data))
 ({ :X a } match-data))

(narc-report)

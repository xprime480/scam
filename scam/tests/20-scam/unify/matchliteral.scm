(import (only (scam base) dict?)
        (only (scam unify) match)
        (test narc))

(narc-label "Match Literal")

(define match-data (match 'a 'a))

(narc-expect
 (#t (dict? match-data))
 (0  (match-data :length)))

(narc-report)

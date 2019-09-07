(import (only (scam unify)
              match)
        (test narc))

(narc-label "Match Empty")

(define match-data (match () ()))

(narc-expect
 (#t (dict? match-data))
 (0  (match-data :length)))

(narc-report)

(import (only (scam unify)
              match)
        (test narc))

(narc-label "Match Var Dup True")

(narc-expect
 ({ :X a } (match '(:X :X) '(a a))))

(narc-report)

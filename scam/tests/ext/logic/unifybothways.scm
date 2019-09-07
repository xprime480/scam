(import (only (scam unify)
              unify)
        (test narc))

(narc-label "Unify Two Ways")

(narc-expect
 ({ :X 3 :Y 2 } (unify '(:X 2) '(3 :Y))))

(narc-report)

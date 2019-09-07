(import (only (scam unify)
              unify)
        (test narc))

(narc-label "Unify Empty")

(narc-expect
 ({} (unify () ())))

(narc-report)

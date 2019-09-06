(import (test narc))

(narc-label "Unify Empty")

(narc-expect
 ({} (unify () ())))

(narc-report)

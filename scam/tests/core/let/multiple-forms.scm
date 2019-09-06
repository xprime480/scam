(import (test let-suite)
        (test narc))

(narc-label "Let Multiple Forms")

(let-suite-expect 9 () 3 5 9)

(narc-report)


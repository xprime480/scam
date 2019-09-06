(import (test let-suite)
        (test narc))

(narc-label "Let Multiple Bindings")

(let-suite-expect 15 ((a 3) (b 5)) (* a b))

(narc-report)


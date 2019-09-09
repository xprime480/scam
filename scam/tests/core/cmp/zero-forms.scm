(import (only (scheme base) = <> < <= > >=)
        (test narc))

(narc-label "CMP Zero Forms")

(narc-expect
 (#t (=))
 (#t (<>))
 (#t (<))
 (#t (<=))
 (#t (>))
 (#t (>=)))

(narc-report)


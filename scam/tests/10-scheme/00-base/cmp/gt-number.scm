(import (only (scheme base) >)
        (test narc))

(narc-label "CMP > Numbers")

(narc-expect
 (#t (> 3))
 (#f (> 3 3))
 (#t (> 3 2))
 (#t (> 4 3 2 1))
 (#f (> 4 3 2 5))
 (#f (> 2+i 1+i))
 (#f (> 1+i 2+i)))

(narc-report)


(import (lib test narc))

(narc-label "CMP < Numbers")

(narc-expect
 (#t (< 3))
 (#f (< 3 3))
 (#t (< 2 3))
 (#t (< 1 2 3 4))
 (#f (< 5 2 3 4))
 (#f (< 2+i 1+i))
 (#f (< 1+i 2+i)))

(narc-report)


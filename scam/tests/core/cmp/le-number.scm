(import (lib test narc))

(narc-label "CMP <= Numbers")

(narc-expect
 (#t (<= 3))
 (#t (<= 3 3))
 (#f (<= 3 2))
 (#t (<= -4 3 3 51))
 (#f (<= 10 1 1))
 (#f (<= 2+i 1+i))
 (#f (<= 1+i 2+i))
 (#t (<= 1+i 1+i)))

(narc-report)


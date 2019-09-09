(import (only (scheme base) max)
        (test narc))

(narc-label "Max")

(narc-expect
 (123   (max 123 -123))
 (42.01 (max 42.01 17.5))
 (-5    (max -5 -9.999)))

(narc-report)

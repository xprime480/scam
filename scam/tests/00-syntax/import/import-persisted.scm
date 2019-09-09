(import (lib2)
        (test narc))

(narc-label "Library Import")

(narc-expect
 (4 (x)))

(narc-report)

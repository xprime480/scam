(import (only lib/prelude min))

(narc-label "Min")

(narc-expect
 (-123   (min 123 -123))
 (17.5   (min 42.01 17.5))
 (-9.999 (min -5 -9.999)))

(narc-report)

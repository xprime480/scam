(import (test narc))

(narc-label "Backtrack without AMB")

(narc-catch
 (:values (backtrack)))

(narc-report)

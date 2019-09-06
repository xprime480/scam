(import (test narc))

(narc-label "AMB Without Values")

(narc-catch
 (:values (amb)))

(narc-report)

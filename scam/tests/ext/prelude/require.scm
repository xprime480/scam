(import (only (scam backtrack)
              require)
        (test narc))

(narc-label "Require")

(narc-skip
 (#t                (require #t))
 ("No more choices" (require #f)))

(narc-expect
 (1 1))

(narc-report)

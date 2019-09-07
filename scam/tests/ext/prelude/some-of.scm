(import (only (scam misc)
              some-of)
        (test narc))

(narc-label "Some-Of")

(narc-skip
 ("No more choices" (some-of '()))
 ('(1)              (some-of '(1)))
 ("No more choices" ?))

(narc-expect
 (1 1))

(narc-report)

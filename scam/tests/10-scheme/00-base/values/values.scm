(import (only (scheme base) values)
        (test narc))

(narc-label "Values")

(narc-expect
 (#f (values))
 (#t (values #t))
 (1  (values 1 2)))

(narc-report)

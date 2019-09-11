(import (only (scheme base) pair?)
        (test narc))

(narc-label "List Literals with Loop")

(narc-expect
 (#t (pair? '#0=(1 . #0#)))
 (1 1))

(narc-report)

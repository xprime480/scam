(import (only (scheme base) +)
        (test narc))

(narc-label "Quote")

(narc-expect
 ('(+ 2 2) (quote (+ 2 2)))
 ('(+ 2 2) '(+ 2 2)))

(narc-report)

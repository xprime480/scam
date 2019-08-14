(import (only lib/prelude filter even?))

(narc-label "Filter")

(narc-expect
 ('()    (filter even? '()))
 ('(2 4) (filter even? '(1 2 3 4 5))))

(narc-report)

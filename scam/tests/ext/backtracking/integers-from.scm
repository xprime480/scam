(import (scam extra numeric)
        (test value-helper)
        (test narc))

(narc-label "Integers From")

(define helper (ValueHelper))

(helper update (integers-from 2))
(backtrack)
(helper update :cat)
(backtrack)

(narc-expect
 ("2 3 :cat 4 " (helper get)))

(narc-report)

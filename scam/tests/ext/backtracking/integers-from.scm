(import lib/prelude lib/numeric lib/test/value_helper)

(narc-label "Integers From")

(define helper (ValueHelper))

(helper update (integers-from 2))
(backtrack)
(helper update :cat)
(backtrack)

(narc-expect
 ("2 3 :cat 4 " (helper get)))

(narc-report)

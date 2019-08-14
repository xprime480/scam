(import lib/prelude lib/numeric lib/test/value_helper)

(narc-label "Integers Between")

(define helper (ValueHelper))

(helper update (integers-between 1 3))
(backtrack)
(helper update :cat)
(backtrack)

(narc-catch
 (:values (backtrack)))

(narc-expect
 ("1 2 :cat 3 " (helper get)))

(narc-report)

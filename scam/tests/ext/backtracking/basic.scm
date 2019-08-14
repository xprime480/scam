(import lib/test/value_helper)

(narc-label "AMB basic")

(define helper (ValueHelper))

(helper update (amb 1 2))
(backtrack)

(narc-catch
 (:values (backtrack)))

(define expected "1 2 ")

(narc-expect
 (expected (helper get)))

(narc-report)

(narc-label "AMB basic")

(load "lib/test/value_helper.scm")

(define helper (ValueHelper))

(helper update (amb 1 2))
(backtrack)

(narc-catch
 (:values (backtrack)))

(define expected "1 2 ")

(narc-expect
 (expected (helper get)))

(narc-report)

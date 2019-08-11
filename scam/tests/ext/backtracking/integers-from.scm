;;; Generate integers between lower and upper bounds.
;;;

(narc-label "Integers From")

(load "lib/prelude.scm")
(load "lib/numeric.scm")
(load "lib/test/value_helper.scm")

(define helper (ValueHelper))

(helper update (integers-from 2))
(backtrack)
(helper update :cat)
(backtrack)

(narc-expect
 ("2 3 :cat 4 " (helper get)))

(narc-report)

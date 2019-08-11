;;; Generate integers between lower and upper bounds.
;;;

(narc-label "Integers Between")

(load "lib/prelude.scm")
(load "lib/numeric.scm")
(load "lib/test/value_helper.scm")

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
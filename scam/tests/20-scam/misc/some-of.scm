(import (only (scam backtrack) backtrack)
        (only (scam misc) some-of)
        (test value-helper)
        (test narc))

(narc-label "Some-Of")

(define helper (ValueHelper))

(narc-catch
 (:values (some-of '())))

(helper update (some-of '(1)))

(narc-catch
 (:values ?))

(narc-expect
 ("(1) " (helper get)))

(narc-report)

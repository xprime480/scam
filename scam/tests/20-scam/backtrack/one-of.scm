(import (only (scheme base) list)
        (only (scam backtrack) backtrack one-of)
        (test value-helper)
        (test narc))

(narc-label "One-Of")

(define helper (ValueHelper))

(helper update (one-of (list 2 8 22)))
?
?

(narc-catch
 (:values (one-of (list))))

(narc-expect
 (2 (one-of (list 2)))
 ("2 8 22 " (helper get)))

(narc-report)

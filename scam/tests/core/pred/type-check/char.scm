(import (only (scheme base) char?)
        (test narc))

(narc-label "Type Checker of Characters")

(narc-expect
 (#t (char? #\a))
 (#f (char? "a-string"))
 (#f (char? 2)))

(narc-catch
 (:args (char?))
 (:args (char? #\a #\b)))

(narc-report)

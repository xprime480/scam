(import (only (scheme base) > boolean?)
        (test narc))

(narc-label "Type Checker of Booleans")

(narc-expect
 (#t (boolean? #t))
 (#t (boolean? #f))
 (#f (boolean? 2))
 (#t (boolean? (> 2 3))))

(narc-catch
 (:args (boolean?))
 (:args (boolean? #t #f)))

(narc-report)

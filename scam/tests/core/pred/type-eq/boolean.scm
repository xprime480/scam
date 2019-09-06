(import (only (scheme base) boolean=?)
        (test narc))

(narc-label "Equality of Booleans")

(narc-catch
 (:args (boolean=?))
 (:args (boolean=? #t)))

(narc-expect
 (#t (boolean=? #t #t #t))
 (#t (boolean=? #f #f #f #f))
 (#f (boolean=? #t #f #t))
 (#f (boolean=? #t 2)))

(narc-report)

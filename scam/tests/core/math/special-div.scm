(import (only (scheme base) /)
        (only (scheme inexact) nan?)
        (test narc))

(narc-label "Special Divide")

(narc-expect
 (0       (/ 0 -inf.0))
 (#t      (nan? (/ 2 -inf.0)))
 (+inf.0  (/ +inf.0 2))
 (-inf.0  (/ +inf.0 -2.0123))
 (#t      (nan? (/ +nan.0 2)))
 (#t      (nan? (/ 2 +nan.0)))
 (#t      (nan? (/ -inf.0 +inf.0)))
 (#t      (nan? (/ -inf.0 -inf.0))))

(narc-report)

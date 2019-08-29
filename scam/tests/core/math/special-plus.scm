(import (lib test narc))

(narc-label "Special Plus")

(narc-expect
 (-inf.0  (+ 2 -inf.0))
 (+inf.0  (+ 2 +inf.0))
 (#t      (nan? (+ 2 +nan.0)))
 (#t      (nan? (+ -inf.0 +inf.0)))
 (+inf.0  (+ +inf.0 +inf.0))
 (-inf.0  (+ -inf.0 -inf.0)))

(narc-report)

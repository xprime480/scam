(import (only (scheme inexact)
              nan?)
        (test narc))

(narc-label "Special Times")

(narc-expect
 (-inf.0  (* 2 -inf.0))
 (+inf.0  (* 2 +inf.0))
 (#t      (nan? (* 2 +nan.0)))
 (-inf.0  (* -inf.0 +inf.0))
 (+inf.0  (* +inf.0 +inf.0))
 (+inf.0  (* -inf.0 -inf.0))
 (#t      (nan? (* 1+i 2-nan.0i)))
 (#t      (nan? (* 1+inf.0i 1-inf.0i))))

(narc-report)

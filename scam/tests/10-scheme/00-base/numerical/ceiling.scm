(import (only (scheme base) ceiling exact?)
        (test narc))

(narc-label "Ceiling")

(narc-catch
 (:args (ceiling "x"))
 (:args (ceiling "+nan.0"))
 (:args (ceiling "2+3i")))

(narc-expect
 (#i-1 (ceiling -1.1))
 (0    (ceiling -1/10))
 (0    (ceiling 0))
 (1    (ceiling 1/10))
 (#i2  (ceiling 1.1))
 (#t   (exact? (ceiling #e1.1)))
 (#f   (exact? (ceiling 1.1)))))

(narc-report)

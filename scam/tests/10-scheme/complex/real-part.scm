(import (scheme complex)
        (test narc))

(narc-label "Real Part")

(narc-expect
 (2      (real-part 2+3i))
 (3/2    (real-part 3/2+inf.0i))
 (2      (real-part 2))
 (#i2    (real-part 2.0-5i))
 (+inf.0 (real-part +inf.0+3i)))

(narc-catch
 (:args (real-part "cat")))

(narc-report)

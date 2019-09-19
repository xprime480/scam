(import (scheme complex)
        (test narc))

(narc-label "Imag Part")

(narc-expect
 (3      (imag-part 2+3i))
 (+inf.0 (imag-part 3/2+inf.0i))
 (0      (imag-part 2))
 (-5     (imag-part 2.0-5i))
 (3      (imag-part +inf.0+3i)))

(narc-catch
 (:args (imag-part "cat")))

(narc-report)

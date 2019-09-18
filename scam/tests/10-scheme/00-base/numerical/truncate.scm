(import (only (scheme base) exact? truncate)
        (test narc))

(narc-label "Truncate")

(narc-catch
 (:args (truncate "x"))
 (:args (truncate "+nan.0"))
 (:args (truncate "2+3i")))

(narc-expect
 (#i-1 (truncate -1.9))
 (#i-1 (truncate -1.1))
 (0    (truncate 1/10))
 (0    (truncate 0))
 (0    (truncate 1/10))
 (#i1  (truncate 1.1))
 (#t   (exact? (truncate #e1.1)))
 (#f   (exact? (truncate #i1.1)))))

(narc-report)

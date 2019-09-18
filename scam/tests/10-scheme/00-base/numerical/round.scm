(import (only (scheme base) exact? round)
        (test narc))

(narc-label "Round")

(narc-catch
 (:args (round "x"))
 (:args (round "+nan.0"))
 (:args (round "2+3i")))

(narc-expect
 (#i-2 (round -1.9))
 (#i-1 (round -1.1))
 (0    (round -1/10))
 (0    (round 0))
 (0    (round 1/10))
 (#i1  (round 1.1))
 #;(#i0  (round 0.5))
 #;(#i2  (round 1.5))
 (#t (exact? (round #e1.1)))
 (#f (exact? (round #i1.1)))))

(narc-report)

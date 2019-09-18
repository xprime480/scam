(import (only (scheme base) exact? floor)
        (test narc))

(narc-label "Floor")

(narc-catch
 (:args (floor "x"))
 (:args (floor "+nan.0"))
 (:args (floor "2+3i")))

(narc-expect
 (#i-2 (floor -1.1))
 (-1   (floor -1/10))
 (0    (floor 0))
 (0    (floor 1/10))
 (#i1  (floor 1.1))
 (#t   (exact? (floor #e1.1)))
 (#f   (exact? (floor #i1.1)))))

(narc-report)












(import (only (scheme base) *)
        (only (checks) z-check)
        (test narc))

(narc-label "Integer Times")

(narc-expect
 ('(1 #t #t)     (z-check (*)))
 ('(2 #t #t)     (z-check (* 2)))
 ('(6 #t #t)     (z-check (* 2 3)))
 ('(48 #t #t)    (z-check (* 2 2 -1 -3 4)))
 ('(0 #t #t)     (z-check (* 2 2 -1 0 4)))
 ('(48.0 #t #f)  (z-check (* 2 2 -1 -3 4.0))))

(narc-report)

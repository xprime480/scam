(import (only (scheme base) +)
        (only (checks) z-check)
        (test narc))

(narc-label "Integer Plus")

(narc-expect
 ('(0 #t #t)    (z-check (+)))
 ('(2 #t #t)    (z-check (+ 2)))
 ('(4 #t #t)    (z-check (+ 2 2)))
 ('(4 #t #t)    (z-check (+ 2 2 -1 -3 4)))
 ('(4.0 #t #f)  (z-check (+ 2 2 -1 -3 4.0))))

(narc-report)

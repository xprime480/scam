(import (only (scheme base) +)
        (only (checks) q-check)
        (test narc))

(narc-label "Rational Plus")

(narc-expect
 ('(5/2 #t #f #t)  (q-check (+ 5/2)))
 ('(1 #t #t #t)    (q-check (+ 2/3 1/3)))
 ('(5/3 #t #f #t)  (q-check (+ 4/3 1/3)))
 ('(1.0 #t #t #f)  (q-check (+ #i1/3 1/3 1/3))))

(narc-report)

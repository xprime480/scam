(import (only (scheme base) /)
        (only (checks) q-check)
        (test narc))

(narc-label "Rational Divide")

(narc-expect
 ('(21/110 #t #f #t)  (q-check (/ 3/10 11/7)))
 ('(#i3/2 #t #f #f)   (q-check (/ 1.5 1)))
 ('(1 #t #t #t)       (q-check (/ 2/3 4/6)))
 ('(#i9/8 #t #f #f)   (q-check (/ 1/2 #i2/3 2/3))))

(narc-report)

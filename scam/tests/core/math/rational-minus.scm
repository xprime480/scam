(import (only checks q-check))

(narc-label "Rational Minus")

(narc-expect
 ('(-5/2 #t #f #t)   (q-check (- 5/2)))
 ('(1/3 #t #f #t)    (q-check (- 2/3 1/3)))
 ('(1 #t #t #t)      (q-check (- 4/3 1/3)))
 ('(#i-1/3 #t #f #f) (q-check (- #i1/3 1/3 1/3))))

(narc-report)

(import (only checks q-check)
        (test narc))

(narc-label "Rational Times")

(narc-expect
 ('(5/2 #t #f #t)    (q-check (* 5/2)))
 ('(2/9 #t #f #t)    (q-check (* 2/3 1/3)))
 ('(1 #t #t #t)      (q-check (* 4/3 3/4)))
 ('(#i1/27 #t #f #f) (q-check (* #i1/3 1/3 1/3))))

(narc-report)

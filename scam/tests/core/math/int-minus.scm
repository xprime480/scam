(import (only checks z-check)
        (test narc))

(narc-label "Integer Minus")

(narc-expect
 ('(0 #t #t)   (z-check (-)))
 ('(-2 #t #t)  (z-check (- 2)))
 ('(5 #t #t)   (z-check (- 7 2)))
 ('(4 #t #t)   (z-check (- 2 2 -1 -3)))
 ('(#i4 #t #f) (z-check (- 2 2 -1 -3.0))))

(narc-report)

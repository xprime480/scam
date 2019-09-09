(import (only (scheme base) /)
        (only (checks) q-check)
        (test narc))

(narc-label "Integer Divide")

(narc-expect
 ('(1 #t #t #t)       (q-check (/)))
 ('(1/2 #t #f #t)     (q-check (/ 2)))
 ('(7/2 #t #f #t)     (q-check (/ 7 2)))
 ('(2 #t #t #t)       (q-check (/ 4 2)))
 ('(#i-1/3 #t #f #f)  (q-check (/ 2 2 -3.0))))

(narc-report)

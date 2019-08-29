(import (only checks q-check)
        (lib test narc))

(narc-label "Integer Modulus")

(narc-expect
 ('(0 #t #t #t)   (q-check (%)))
 ('(0 #t #t #t)   (q-check (% 2)))
 ('(1 #t #t #t)   (q-check (% 7 2)))
 ('(1.0 #t #t #f) (q-check (% 7 2.0)))
 ('(33 #t #t #t)  (q-check (% 77 44 0 0)))) ; this is an error in proper scheme!

(narc-report)

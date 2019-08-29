(import (only checks q-check)
        (lib test narc))

(narc-label "Rational Modulus")

(narc-expect
 ('(0.0 #t #t #f)   (q-check (% 12.5 2.5)))
 ('(#i1/4 #t #f #f) (q-check (% 12.75 0.5)))
 ('(1/3 #t #f #t)   (q-check (% 5/3 2/3))))

(narc-report)

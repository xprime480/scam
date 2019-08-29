(import (only checks c-check)
        (lib test narc))

(narc-label "Complex Divide")

(narc-expect
 ('(+i #t #f #f #f #t)       (c-check (/ 1+i 1-i)))
 ('(#i+i #t #f #f #f #f)     (c-check (/ 1+i 1.0-i)))
 ('(3/5+1/5i #t #f #f #f #t) (c-check (/ 1+i 2+i)))
 ('(1 #t #t #t #t #t)        (c-check (/ 1+i 1+i))))

(narc-report)

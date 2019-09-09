(import (only (scheme base) +)
        (only (checks) c-check)
        (test narc))

(narc-label "Complex Plus")

(narc-expect
 ('(+i #t #f #f #f #t)        (c-check (+ +i)))
 ('(2+2i #t #f #f #f #t)      (c-check (+ 1+I 1+I)))
 ('(#i5/2+2i #t #f #f #f #f)  (c-check (+ 1.5+i 1+i)))
 ('(2 #t #t #t #t #t)         (c-check (+ 1+i 1-i)))
 ('(36/7+i #t #f #f #f #t)    (c-check (+ 1+i 1-i 22/7+i))))

(narc-report)

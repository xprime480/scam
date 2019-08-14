(import (only checks c-check))

(narc-label "Complex Times")

(narc-expect
 ('(1+i #t #f #f #f #t) (c-check (* 1+i)))
 ('(5-i #t #f #f #f #t) (c-check (* 1+i 2-3i)))
 ('(2 #t #t #t #t #t)   (c-check (* 1+i 1-i))))

(narc-report)

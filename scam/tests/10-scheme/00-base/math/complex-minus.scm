(import (only (scheme base) -)
        (only (checks) c-check)
        (test narc))

(narc-label "Complex Minus")

(narc-expect
 ('(-i #t #f #f #f #t)        (c-check (- +i)))
 ('(1-2i #t #f #f #f #t)      (c-check (- 2+I 1+3I)))
 ('(#i3/2+2i #t #f #f #f #f)  (c-check (- 1.5+i -i)))
 ('(-1 #t #t #t #t #t)        (c-check (- 1+i 2+i)))
 ('(-15/7+i #t #f #f #f #t)   (c-check (- 2+i 1-i 22/7+i))))

(narc-report)

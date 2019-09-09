(import (only (scheme base) + / equal?)
        (test narc))

(narc-label "Equal for numbers")

(narc-expect
 (#t (equal? 0 0))
 (#f (equal? 0.0 0))
 (#t (equal? 0.0 0.0))
 (#f (equal? #i0 0))

 (#f (equal? 1.5 3/2))
 (#t (equal? 1.5 #i3/2))

 (#t (equal? (/ 2 3) 2/3))
 (#t (equal? (+ 1/2 1/2) 1))
 (#t (equal? 5e-1 #i1/2))

 (#t (equal? 1+i 1+i))
 (#t (equal? -i -i))

 (#t (equal? -inf.0 -inf.0))
 (#t (equal? +inf.0 +inf.0))
 (#f (equal? +nan.0 +nan.0)))

(narc-report)

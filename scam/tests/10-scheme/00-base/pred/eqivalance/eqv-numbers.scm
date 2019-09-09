(import (only (scheme base) + / eqv?)
        (test narc))

(narc-label "Eqv for numbers")

(narc-expect
 (#t (eqv? 0 0))
 (#f (eqv? 0.0 0))
 (#t (eqv? 0.0 0.0))
 (#f (eqv? #i0 0))

 (#f (eqv? 1.5 3/2))
 (#t (eqv? 1.5 #i3/2))

 (#t (eqv? (/ 2 3) 2/3))
 (#t (eqv? (+ 1/2 1/2) 1))
 (#t (eqv? 5e-1 #i1/2))

 (#t (eqv? 1+i 1+i))
 (#t (eqv? -i -i))

 (#t (eqv? -inf.0 -inf.0))
 (#t (eqv? +inf.0 +inf.0))
 (#f (eqv? +nan.0 +nan.0)))

(narc-report)

(import (lib test narc))

(narc-label "Eqv for numbers")

(narc-expect
 ('(#t #f #t #f) (list (eqv? 0 0)
                       (eqv? 0.0 0)
                       (eqv? 0.0 0.0)
                       (eqv? #i0 0)))

 ('(#f #t) (list (eqv? 1.5 3/2)
                 (eqv? 1.5 #i3/2)))

 ('(#t #t #t) (list (eqv? (/ 2 3) 2/3)
                    (eqv? (+ 1/2 1/2) 1)
                    (eqv? 5e-1 #i1/2)))

 ('(#t #t) (list (eqv? 1+i 1+i)
                 (eqv? -i -i)))

 ('(#t #t #f) (list (eqv? -inf.0 -inf.0)
                    (eqv? +inf.0 +inf.0)
                    (eqv? +nan.0 +nan.0))))

(narc-report)

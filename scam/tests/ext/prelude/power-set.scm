(import (only (scam misc)
              power-set)
        (test narc))

(narc-label "Power-Set")

(define pset (power-set '(1 2)))

(narc-expect
 ('(())     (power-set '()))
 ('(() (1)) (power-set '(1)))

 (4  (length pset))
 (#t (member? '()    pset))
 (#t (member? '(1)   pset))
 (#t (member? '(2)   pset))
 (#t (member? '(1 2) pset)))

(narc-report)

(import (only (scheme base) eqv?)
        (test narc))

(narc-label "Eqv for atoms")

(narc-expect
 (#t (eqv? #t #t))
 (#t (eqv? #f #f))
 (#f (eqv? #t #f))

 (#t (eqv? 'sym1 'sym1))
 (#f (eqv? 'sym1 'sym2))

 (#t (eqv? :keyword1 :keyword1))
 (#f (eqv? :keyword1 :keyword2))

 (#t (eqv? #\a #\a))
 (#f (eqv? #\a #\b))

 (#t (eqv? '() '()))
 (#f (eqv? '() 2)))

(narc-report)

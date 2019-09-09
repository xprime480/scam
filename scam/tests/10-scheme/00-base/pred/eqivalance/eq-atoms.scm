(import (only (scheme base) eq?)
        (test narc))

(narc-label "Eq for atoms")

(narc-expect
 (#t (eq? #t #t))
 (#t (eq? #f #f))
 (#f (eq? #t #f))

 (#t (eq? 'sym1 'sym1))
 (#f (eq? 'sym1 'sym2))

 (#t (eq? :keyword1 :keyword1))
 (#f (eq? :keyword1 :keyword2))

 (#t (eq? #\a #\a))
 (#f (eq? #\a #\b))

 (#t (eq? '() '()))
 (#f (eq? '() 2)))

(narc-report)

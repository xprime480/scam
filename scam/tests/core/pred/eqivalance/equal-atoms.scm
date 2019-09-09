(import (only (scheme base) equal?)
        (test narc))

(narc-label "Equal for atoms")

(narc-expect
 (#t (equal? #t #t))
 (#t (equal? #f #f))
 (#f (equal? #t #f))

 (#t (equal? 'sym1 'sym1))
 (#f (equal? 'sym1 'sym2))

 (#t (equal? :keyword1 :keyword1))
 (#f (equal? :keyword1 :keyword2))

 (#t (equal? #\a #\a))
 (#f (equal? #\a #\b))

 (#t (equal? '() '()))
 (#f (equal? '() 2)))

(narc-report)

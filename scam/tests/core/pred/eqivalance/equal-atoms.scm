(import (lib test narc))

(narc-label "Equal for atoms")

(narc-expect
 ('(#t #t #f) (list (equal? #t #t)
                    (equal? #f #f)
                    (equal? #t #f)))

 ('(#t #f) (list (equal? 'sym1 'sym1)
                 (equal? 'sym1 'sym2)))

 ('(#t #f) (list (equal? :keyword1 :keyword1)
                 (equal? :keyword1 :keyword2)))

 ('(#t #f) (list (equal? #\a #\a)
                 (equal? #\a #\b)))

 ('(#t #f) (list (equal? '() '())
                 (equal? '() 2))))

(narc-report)

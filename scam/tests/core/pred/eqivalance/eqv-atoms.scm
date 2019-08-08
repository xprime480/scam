;;; Test the eqv? function on atoms
;;;

(narc-label "Eqv for atoms")

(narc-expect
 ('(#t #t #f) (list (eqv? #t #t)
                    (eqv? #f #f)
                    (eqv? #t #f)))

 ('(#t #f) (list (eqv? 'sym1 'sym1)
                 (eqv? 'sym1 'sym2)))

 ('(#t #f) (list (eqv? :keyword1 :keyword1)
                 (eqv? :keyword1 :keyword2)))

 ('(#t #f) (list (eqv? #\a #\a)
                 (eqv? #\a #\b)))

 ('(#t #f) (list (eqv? '() '())
                 (eqv? '() 2))))

(narc-report)

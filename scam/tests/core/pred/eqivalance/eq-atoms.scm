(import (lib test narc))

(narc-label "Eq for atoms")

(narc-expect
 ('(#t #t #f) (list (eq? #t #t)
		    (eq? #f #f)
		    (eq? #t #f)))

 ('(#t #f) (list (eq? 'sym1 'sym1)
		 (eq? 'sym1 'sym2)))

 ('(#t #f) (list (eq? :keyword1 :keyword1)
		 (eq? :keyword1 :keyword2)))

 ('(#t #f) (list (eq? #\a #\a)
		 (eq? #\a #\b)))

 ('(#t #f)     (list (eq? '() '())
		     (eq? '() 2))))

(narc-report)

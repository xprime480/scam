(import (lib test narc))

(narc-label "Equal for numbers")

(narc-expect
 ('(#t #f #t #f) (list (equal? 0 0)
                       (equal? 0.0 0)
                       (equal? 0.0 0.0)
                       (equal? #i0 0)))

 ('(#f #t) (list (equal? 1.5 3/2)
                 (equal? 1.5 #i3/2)))

 ('(#t #t #t) (list (equal? (/ 2 3) 2/3)
                    (equal? (+ 1/2 1/2) 1)
                    (equal? 5e-1 #i1/2)))

 ('(#t #t) (list (equal? 1+i 1+i)
                 (equal? -i -i)))


 ('(#t #t #f) (list (equal? -inf.0 -inf.0)
                    (equal? +inf.0 +inf.0)
                    (equal? +nan.0 +nan.0))))

(narc-report)

(import (only (scheme base) append)
        (test narc))

(narc-label "Append Lists")

(narc-expect
 ('()          (append))
 ('(x y)       (append '(x) '(y)))
 ('(a b c d)   (append '(a) '(b c d)))
 ('(a (b) (c)) (append '(a (b)) '((c))))
 ('(a b c . d) (append '(a b) '(c . d)))
 ('a           (append '() 'a))
 ('a           (append 'a))
 ('(a b c)     (append '(a b c) '())))

(narc-catch
 (:args  (append 2 3)))

(narc-report)

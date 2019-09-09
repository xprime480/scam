(import (only (scheme base) cdr list)
        (only (scam backtrack) exclude)
        (test narc))

(narc-label "Exclude")

(define x (list 1 2 3))

(narc-expect
 ('()      (exclude () ()))
 ('(1 2 3) (exclude () (list 1 2 3)))
 ('()      (exclude (list 1 2 3) ()))
 ('()      (exclude x x))
 ('(1)     (exclude (cdr x) x))
 ('(3 4)   (exclude (list 1 2) (list 3 4))))

(narc-report)

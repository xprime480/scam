(import (only (scheme base) cons list)
        (test narc))

(narc-label "Create Pairs")

(narc-expect
 ('(1 . 2)  (cons 1 2))
 ('(1)      (cons 1 '()))
 ('()       (list))
 ('(1 2 3)  (list 1 2 3)))

(narc-catch
 (:args (cons))
 (:args (cons 1))
 (:args (cons 1 2 3)))

(narc-report)

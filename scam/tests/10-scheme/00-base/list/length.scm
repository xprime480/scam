(import (only (scheme base) cons exact? length set-cdr!)
        (test narc))

(narc-label "List Length")

(define x (cons 1 1))
(set-cdr! x x)

(narc-expect
 (0  (length ()))
 (3  (length '(1 (sublist counts as 1) 3)))
 (#t (exact? (length '()))))

(narc-catch
 (:args (length))
 (:args (length () ()))
 (:args (length '(a . b)))
 (:args (length x)))

(narc-report)

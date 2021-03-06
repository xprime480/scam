(import (only (scheme base) cons list pair?)
        (test narc))

(narc-label "Type Checker of Pairs")

(narc-expect
 (#t (pair? (cons 1 2)))
 (#t (pair? (list 1 2 3)))
 (#f (pair? 2))
 (#f (pair? '())))

(narc-catch
 (:args (pair?))
 (:args (pair? '(a . b) (list 1 2 3))))

(narc-report)

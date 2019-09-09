(import (only (scheme base) list open-input-string port?)
        (test narc))

(narc-label "Type Checker of Ports")

(narc-expect
 (#f (port? 2))
 (#t (port? (open-input-string ""))))

(narc-catch
 (:args (port?))
 (:args (port? '(a . b) (list 1 2 3))))

(narc-report)

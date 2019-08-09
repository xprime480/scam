;;; Test the null? function
;;;

(narc-label "Type Checker of Null")

(narc-expect
 (#t (null? ()))
 (#f (null? 2)))

(narc-catch
 (:args (null?))
 (:args (null? '() '())))

(narc-report)

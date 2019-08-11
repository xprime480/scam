;;; Generate integers between lower and upper bounds.
;;;

(narc-label "Integers Between")

(load "lib/prelude.scm")
(load "lib/numeric.scm")

(define port (open-output-string))
(define foo (lambda (x)
              (display x port)
              (display " " port)))

(foo (integers-between 1 3))
(backtrack)
(foo :cat)
(backtrack)

(narc-catch
 (:values (backtrack)))

(narc-expect
 ("1 2 :cat 3 " (get-output-string port)))

(narc-report)

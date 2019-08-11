;;; Generate integers between lower and upper bounds.
;;;

(narc-label "Integers From")

(load "lib/prelude.scm")
(load "lib/numeric.scm")

(define port (open-output-string))
(define foo (lambda (x)
              (display x port)
              (display " " port)))

(foo (integers-from 2))
(backtrack)
(foo :cat)
(backtrack)

(narc-expect
 ("2 3 :cat 4 " (get-output-string port)))

(narc-report)

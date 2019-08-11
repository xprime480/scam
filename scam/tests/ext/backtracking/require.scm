;;; Test the require function in the prelude
;;;

(narc-label "Require")

(load "lib/prelude.scm")
(load "lib/test/value_helper.scm")

(define helper (ValueHelper))

(helper update (let ((x (amb 1 2 3 4 5 6)))
                 (begin
                   (require (even? x))
                   x)))
(backtrack)
(backtrack)

(narc-catch
 (:values (backtrack)))

(narc-expect
 ("2 4 6 " (helper get)))

(narc-report)

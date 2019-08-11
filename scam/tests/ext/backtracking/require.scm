;;; Test the require function in the prelude
;;;

(narc-label "Require")

(load "lib/prelude.scm")

(define port (open-output-string))
(define foo (lambda (x)
              (display x port)
              (display " " port)))

(foo (let ((x (amb 1 2 3 4 5 6)))
       (begin
         (require (even? x))
         x)))
(backtrack)
(backtrack)

(narc-catch
 (:values (backtrack)))

(narc-expect
 ("2 4 6 " (get-output-string port)))

(narc-report)
